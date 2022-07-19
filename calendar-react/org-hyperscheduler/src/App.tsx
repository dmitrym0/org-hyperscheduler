import * as React from "react";
import { useState, useEffect, useRef} from 'react'
import logo from './logo.svg'
import './App.css'

// Bulma React Reference: https://react-bulma.dev/en/storybook
// Bulma Full Ref: https://bulma.io/documentation/components/navbar/
import 'bulma/css/bulma.min.css';
import { Navbar, Level, Container,Link,  Button, Table } from 'react-bulma-components';

import { createWebStoragePersistor } from "react-query/createWebStoragePersistor-experimental"

import { persistQueryClient } from 'react-query/persistQueryClient-experimental'

import { useCallback  } from 'react';


import TUICalendar from "@toast-ui/react-calendar";

// import "tui-calendar/dist/tui-calendar.css";
// import "tui-date-picker/dist/tui-date-picker.css";
// import "tui-time-picker/dist/tui-time-picker.css";


import { ReactQueryDevtools } from "react-query/devtools";

import "tui-calendar/dist/tui-calendar.css";
import "tui-date-picker/dist/tui-date-picker.css";
import "tui-time-picker/dist/tui-time-picker.css";

import '@toast-ui/calendar/dist/toastui-calendar.min.css';




import {
  useQuery,
  useMutation,
  useQueryClient,
  QueryClient,
  QueryClientProvider,
} from 'react-query'



const cacheTime = 1000000000000 * 5;

export const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      cacheTime: cacheTime
    },
  },
})

// convert javascript date to unix time stamp because that's easier to pass around.
function getUnixTimestampFromDate(date) {
    return date.getTime() / 1000;
}


const localStoragePersistor = createWebStoragePersistor({
  storage: window.localStorage,
})


persistQueryClient({
  queryClient,
  persistor: localStoragePersistor,
  maxAge: cacheTime,
})


const bindWebSocket = () => {
  console.log("[websocket] Initializing..");
  const websocket = new WebSocket("ws://127.0.0.1:44445");

  websocket.onopen = () => {
    console.log('[websocket] Connected')

  }


    websocket.onmessage = (event) => {
        console.log("[websocket] Global websocket handler");
        const payload = JSON.parse(event.data);

        if (payload?.command) {
            console.log(`[websocket] Got command =${payload.command}=`);
            if (payload.command === 'invalidate') {
                queryClient.invalidateQueries(['agenda']);
            }
        }
    }

  return websocket;

}

const websocket = bindWebSocket()

const fetchAgenda = () => {
  return syncSend({ "command": "get-agenda" });
}

// a helper that turns websocket messaging into a synchronous affair.
const syncSend = (payload) => {
    console.log(`[websocket] syncSend ${JSON.stringify(payload)}`);
    const string_payload = JSON.stringify(payload);
    websocket.send(string_payload);
    var promise = new Promise(function(resolve, reject) {
        let syncResponseHandler = (event) => {
            console.log(`[websocket] Response for ${string_payload}`);
            const data = JSON.parse(event.data);
            resolve(data);
            websocket.removeEventListener('message', syncResponseHandler);
        }
        websocket.addEventListener('message', syncResponseHandler);
    });
    return promise;
}



const useNewEvent = (payload) => useMutation(
    () => {
        const string_payload = JSON.stringify(payload);
        websocket.send(payload);
    },
    {
        onSuccess: () => {
            queryClient.invalidateQueries(['agenda'])
        },
    }
);


const useAgenda = () => useQuery(["agenda"],
  fetchAgenda,
  {
      // in milliseconds
      // docs here: https://react-query-v3.tanstack.com/guides/important-defaults
      //
      // Query instances via useQuery or useInfiniteQuery by default consider cached data as stale. staleTime configures that.
      staleTime: 60 * 60 * 1000,
      // Query results that have no more active instances of useQuery, useInfiniteQuery or query observers are labeled as "inactive"
      // and remain in the cache in case they are used again at a later time. cacheTime configures this.
      cacheTime: 60 * 60 * 1000,
  });

const calendars = [
  {
    id: '1',
    name: 'Scheduled Items',
    backgroundColor: '#9e5fff',
    dragBgColor: '#9e5fff',
    borderColor: '#9e5000',
    customStyle: {'fontSize': '99px'}
  },
  {
    id: '2',
    name: 'Timestamped Items',
    backgroundColor: '#00a9ff',
    dragColor: '#00a9ff',
    borderColor: '#00a9ff'
  }
];


// take the JSON that's sent via websocket and turn it into something TUI Calendar will understand.
const parseAgenda = (agenda) => {
    console.log("Parsing agenda...");
    const schedule = [];

    // so nice
    if (Object.keys(agenda)?.at(0) === 'agenda') {
        agenda = agenda.agenda;
    }

    if (agenda === undefined) {
        console.warn("Empty agenda!");
        return;
    }

    console.log(`${agenda.length} items in agenda.`);
    for (const agendaItem of agenda) {
        let calendarItem = {
            id: agendaItem["ID"],
            calendarId: "1",
            dueDateClass: '',
            // sad attempt at removing links. *TODO*
            title: agendaItem["ITEM"].replaceAll(/\[\[.*:.*\]\[/ig, '').replaceAll(/\]\]/ig, ''),
            category: 'time',
            start: agendaItem["startDate"],
            end: agendaItem["endDate"],
            //isReadOnly: agendaItem["isReadOnly"],
            //isAllDay: agendaItem["allDay"]
        };


        if (agendaItem["allDay"] === "true") {
            calendarItem.category = 'allday';
        }

        if (agendaItem["SCHEDULED"] === undefined) {
            calendarItem.calendarId = "2";
        }
        schedule.push(calendarItem);
    }


    return schedule;
}



export function ReactCalendar(props) {
    console.log("Rendering ReactCalendar");


    const [calendarView, onCalendarViewChange] = useState("week");
    const [agenda, setAgenda] = useState([]);


    const calendarRef = useRef(null);

    const getCalInstance = useCallback(() => calendarRef.current?.getInstance?.(), []);

    const onBeforeUpdateSchedule = (payload) => {
        const event = payload.event;
        let changes = payload.changes;
        if (changes.start === undefined) {
            changes.start = updated_schedule.start;
        }

        console.log(`Time changed to ${getUnixTimestampFromDate(changes.end)}`);

        let update_event_data = {id: event.id, start: getUnixTimestampFromDate(changes.start), end: getUnixTimestampFromDate(changes.end)};
        websocket.send(JSON.stringify({"command": "update-event", "data":update_event_data}));
        getCalInstance().updateEvent(event.id, event.calendarId, changes);
    };


    const updateSchedule = (agenda) => {
        console.log("Updating schedule.");
        if (agenda !== undefined && getCalInstance()) {
            getCalInstance().clear();
            getCalInstance().createEvents(agenda);
            console.log("Schedule updated.");
        }
    }

    const { isLoading, error, data } = useAgenda();

    if (isLoading) {
        return <div> Loading </div>
    } else {
        let parsedAgenda = parseAgenda(data);
        // TODO: this needs to be refactored. Neither the useEffect, nor useState are necessary for agenda because it comes
        // from react query. I can't quite figure out how to force a refresh cleanly though.
        useEffect(() => {
            if (parsedAgenda) {
                console.log("Setting initial agenda.");
                setAgenda(parsedAgenda);
            }
        }, []);
        updateSchedule(parsedAgenda);
    }

    const eventStyle = {
        overflowWrap: "break-word",
        fontStyle: 'italic',
        fontSize: '15px',
    }

    return (
            <div>

            <Navbar>
            <Navbar.Menu>
            <Navbar.Container>
            <Navbar.Item>
            <Button.Group hasAddons={true}>
            <Button color="primery" renderAs={Link} onClick={() => getCalInstance().prev() } >Previous</Button>
            <Button color="primery" renderAs={Link} onClick={() => getCalInstance().today() } >Today</Button>
            <Button color="primery" renderAs={Link} onClick={() => getCalInstance().next() } >Next</Button>

        </Button.Group>
            </Navbar.Item>
            </Navbar.Container>

            <Navbar.Container align="end">

            <Navbar.Item>
            <Button.Group hasAddons={true}>
            <Button color="primery" renderAs={Link} onClick={() => onCalendarViewChange('day')}>Day</Button>
            <Button color="primery" renderAs={Link} onClick={() => onCalendarViewChange('week')}>Week</Button>
            <Button color="primery" renderAs={Link} onClick={() => onCalendarViewChange('month')}>Month</Button>
            </Button.Group>
            </Navbar.Item>
            </Navbar.Container>
            </Navbar.Menu>
            </Navbar>

            <Container>
            <TUICalendar
        height="800px"
        usageStatistics={false}
        isReadOnly={false}
        ref={calendarRef}
        useCreationPopup={true}
        useDetailPopup={true}
        calendars={calendars}
        month={{ startDayOfWeek: 1 }}
        view={calendarView}
        week={{
            showTimezoneCollapseButton: false,
            timezonesCollapsed: false,
            eventView: true,
            taskView: false,
            startDayOfWeek: 1
        }}
        template={{
            time(event) {
                return `<div style="word-wrap: break-word;">${event.title}</div>`;
            },
        }}
        timezones={[
            {
                timezoneOffset: 540,
                displayLabel: 'GMT+09:00',
                tooltip: 'Seoul',
            },
            {
                timezoneOffset: -420,
                displayLabel: 'GMT-08:00',
                tooltip: 'Los Angeles',
            },
        ]}


        // onClickSchedule={onClickSchedule}
        // onBeforeCreateSchedule={onBeforeCreateSchedule}
        // onBeforeDeleteSchedule={onBeforeDeleteSchedule}
        //onBeforeUpdateSchedule={onBeforeUpdateSchedule}
        onBeforeUpdateEvent={onBeforeUpdateSchedule}
            />
            </Container>
            </div>
    );
}



export default function App() {

  return (
    <QueryClientProvider client={queryClient}>
      <ReactCalendar />
      <ReactQueryDevtools  />
    </QueryClientProvider>
  )
}
