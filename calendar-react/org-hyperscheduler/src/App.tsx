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



// in milliseconds
const cacheTime = 1000 * 10000;

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

////////////////////////
// calendar component //
////////////////////////
export function ReactCalendar(props) {

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
                else if (payload.command === 'update-settings') {
                    console.log(payload);
                    onSettingsChange(payload.data);
                }
            }
        }

        return websocket;

    }

    // this seems pretty dumb
    let websocket;
    useEffect(() => {
        websocket = bindWebSocket();
    }, []);


    const fetchAgenda = () => {
        return syncSend({ "command": "get-agenda" });
    }

    const fetchSettings = () => {
        return syncSend({ "command": "get-settings" });
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
                console.log(data);
                resolve(data);
                websocket.removeEventListener('message', syncResponseHandler);
            }
            websocket.addEventListener('message', syncResponseHandler);
        });
        return promise;
    }



    // send througha  new event via react query
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
            staleTime: 5 * 60 * 1000,
            // Query results that have no more active instances of useQuery, useInfiniteQuery or query observers are labeled as "inactive"
            // and remain in the cache in case they are used again at a later time. cacheTime configures this.
            cacheTime: 5 * 60 * 1000,
        });

    const useSettings = () => useQuery(["settings"],
        fetchSettings,
        {
            staleTime: 5 * 60 * 1000,
            cacheTime: 5 * 60 * 1000,
        });


    const calendars = [
        {
            id: '1',
            name: 'Scheduled Items',
            backgroundColor: '#9e5fff',
            dragBgColor: '#9e5fff',
            borderColor: '#9e5000',
            customStyle: { 'fontSize': '99px' }
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

        if (agenda === undefined) {
            console.warn("Empty agenda!");
            return;
        }

        // so nice
        if (Object.keys(agenda)?.at(0) === 'agenda') {
            agenda = agenda.agenda;
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



    console.log("Rendering ReactCalendar");

    const [settings, onSettingsChange] = useState({
      "defaultCalendarView": "week",
      "showDone": true,
      "showClocked": true
    });
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

    let parsedAgenda = parseAgenda(data);
    updateSchedule(parsedAgenda);

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
                                <Button color="primery" renderAs={Link} onClick={() => getCalInstance().prev()} >Previous</Button>
                                <Button color="primery" renderAs={Link} onClick={() => getCalInstance().today()} >Today</Button>
                                <Button color="primery" renderAs={Link} onClick={() => getCalInstance().next()} >Next</Button>

                            </Button.Group>
                        </Navbar.Item>
                    </Navbar.Container>

                    <Navbar.Container align="end">

                        <Navbar.Item>
                            <Button.Group hasAddons={true}>
                                <Button color="" renderAs={Link} onClick={() => onSettingsChange({"defaultCalendarView": "day"})}>Day</Button>
                                <Button color="" renderAs={Link} onClick={() => onSettingsChange({ "defaultCalendarView": "week" })}>Week</Button>
                                <Button color="" renderAs={Link} onClick={() => onSettingsChange({ "defaultCalendarView": "month" })}>Month</Button>
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
                    events={parsedAgenda}
                    month={{ startDayOfWeek: 1 }}
                    view={settings.defaultCalendarView}
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
