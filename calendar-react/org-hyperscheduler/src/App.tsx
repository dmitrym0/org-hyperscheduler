import * as React from "react";
import { useState, useEffect, useRef} from 'react'
import logo from './logo.svg'
import './App.css'

// Bulma React Reference: https://react-bulma.dev/en/storybook
// Bulma Full Ref: https://bulma.io/documentation/components/navbar/
import 'bulma/css/bulma.min.css';
import { Footer, Navbar, Level, Container,Link,  Button, Table, Modal, Content, Box } from 'react-bulma-components';

import { createWebStoragePersistor } from "react-query/createWebStoragePersistor-experimental"

import { persistQueryClient } from 'react-query/persistQueryClient-experimental'

import { useCallback  } from 'react';


import TUICalendar from "@toast-ui/react-calendar";

// import "tui-calendar/dist/tui-calendar.css";
// import "tui-date-picker/dist/tui-date-picker.css";
// import "tui-time-picker/dist/tui-time-picker.css";


// import { ReactQueryDevtools } from "react-query/devtools";

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

    const invalidateCachedAgenda = () => {
        queryClient.invalidateQueries(['agenda']);
    }

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
                    invalidateCachedAgenda();
                }
                else if (payload.command === 'update-settings') {
                    console.log(payload);
                    onSettingsChange(payload.data);
                }
            }
        }

        return websocket;

    }

    const [websocket, setWebsocket] = useState(bindWebSocket);

    const fetchAgenda = () => {
        return syncSend({ "command": "get-agenda" });
    }

    const fetchSettings = () => {
        return syncSend({ "command": "get-settings" });
    }


    const asyncSend = (payload) => {
        console.log(`[websocket] asyncSend ${JSON.stringify(payload)}`);
        const string_payload = JSON.stringify(payload);
        websocket.send(string_payload);
    };

    // a helper that turns websocket messaging into a synchronous affair.
    const syncSend = (payload) => {
        console.log(`[websocket] syncSend ${JSON.stringify(payload)}`);
        const string_payload = JSON.stringify(payload);
        websocket.send(string_payload);
        var promise = new Promise(function(resolve, reject) {
            const syncResponseHandler = (event) => {
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
    const useNewEvent = useMutation(payload => {
        const string_payload = JSON.stringify(payload);
        return syncSend(payload);
    },
    {
        onSuccess: (event) => {
            queryClient.setQueryData(['agenda', event.id], event)
        },
    }
    );


    const updateEvent = useMutation(existingEvent => {
        asyncSend(existingEvent);
        console.log("Sending an update");
    },
                                    {
        onMutate: async existingEvent => {
            // Cancel any outgoing refetches (so they don't overwrite our optimistic update)
            await queryClient.cancelQueries(['agenda', existingEvent.id])

            // Snapshot the previous value
            const ag = queryClient.getQueryData(['agenda']).agenda;

            console.log("On mutate");

            // walk all the events, when we find the one that matches
            // update it which will force a local UI.
            const updatedAgenda = ag.map(function (event, index) {
                if (event.ID === existingEvent.data.id) {
                    console.log("Found the entry to mutate.");
                    const updatedEvent = event;
                    updatedEvent.startDate = existingEvent.data.startjs;
                    updatedEvent.endDate= existingEvent.data.endjs;
                    return updatedEvent;
                }
                return event;
            });



            // Optimistically update to the new value
            queryClient.setQueryData(['agenda'], {agenda: updatedAgenda});

            // Return a context with the previous and new todo
            return {agenda: ag};
        }

    })

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
        },
        {
            id: '3',
            name: 'Clocked Entries',
            backgroundColor: '#DCDCDC',
            dragColor: '#00a9ff',
            borderColor: '#00a9ff'
        },
        {
            id: '4',
            name: 'Done Entries',
            backgroundColor: '#848482',
            dragColor: '#00a9ff',
            borderColor: '#00a9ff'
        }


    ];


    // take the JSON that's sent via websocket and turn it into something TUI Calendar will understand.
    const parseAgenda = (agenda) => {
        console.log("Parsing agenda...");
        const schedule = [];

        console.log(agenda);

        if (agenda === undefined) {
            console.warn("Empty agenda!");
            return;
        }

        // so nice
        if (Object.keys(agenda)[0] === 'agenda') {
            agenda = agenda.agenda;
        }

        if (agenda.length === undefined) {
          console.warn("Argh, bad agenda!");
          return;
        }

        console.log(`${agenda.length} items in agenda.`);

//        console.groupCollapsed("agenda");

        for (let i = 0; i < agenda.length; i++) {
            const agendaItem = agenda[i];
            // console.log(`${i} ${agendaItem["ITEM"]}`);
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

            if (agendaItem.TODO === "DONE") {
                calendarItem.calendarId = "4";
            }

            if (agendaItem.clockedList.length) {
                for (const clockedItem of agendaItem.clockedList) {
                    let clockedEntry = {
                        id: agendaItem["ID"],
                        calendarId: "3",
                        // sad attempt at removing links. *TODO*
                        title: agendaItem["ITEM"].replaceAll(/\[\[.*:.*\]\[/ig, '').replaceAll(/\]\]/ig, ''),
                        category: 'time',
                        start: clockedItem["startDate"],
                        end: clockedItem["endDate"],
                        isReadOnly: true
                    };
                    schedule.push(clockedEntry);
                }
            }

            if (i === 15) {
                console.log("asdf");
                // debugger;
            }
            // dont show done tasks when it has clocked entries
            // dont show entries that don't have a start/end date (these are just clocked entries)
            if (!isDoneTaskWithClockedEntries(calendarItem) &&
                !isUnscheduledEntry(calendarItem)) {
                schedule.push(calendarItem);
            }
        }

//        console.groupEnd();


        return schedule;
    }

    const isDoneTaskWithClockedEntries = (calendarItem) => {
        return (calendarItem.TODO === "DONE" && !calendarItem?.clockedList?.length);
    }

    const isUnscheduledEntry = (calendarItem) => {
        return !(calendarItem.start && calendarItem.end);
    }


    console.log("Rendering ReactCalendar");

    const [settings, onSettingsChange] = useState({
        "defaultCalendarView": "week",
        "showDone": true,
        "showClocked": true,
        "readOnly": false
    });

    const [calendarView, onCalendarViewChange] = useState("week");
    const [agenda, setAgenda] = useState([]);

    const calendarRef = useRef(null);

    const getCalInstance = useCallback(() => calendarRef.current?.getInstance?.(), []);

    const onBeforeUpdateSchedule = (payload) => {
        const event = payload.event;
        let changes = payload.changes;
        const ch = {...payload.changes};

        // if we change the event by dragging the bottom handle in the UI we only get the end date.
        if (changes.start === undefined) {
            changes.start = event.start;
        }

        console.log(`Time changed to ${getUnixTimestampFromDate(changes.end)}`);

        let update_event_data = {id: event.id, startjs: changes.start, endjs: changes.end, start: getUnixTimestampFromDate(changes.start), end: getUnixTimestampFromDate(changes.end)};
        getCalInstance().updateEvent(event.id, event.calendarId, ch);
        updateEvent.mutate({"command": "update-event", "data":update_event_data});
        //asyncSend({"command": "update-event", "data":update_event_data});

    };

    const onBeforeCreateEvent = (event) => {
        event.startUnix = getUnixTimestampFromDate(event.start);
        event.endUnix =  getUnixTimestampFromDate(event.end);

        // optimistic add. add the even to the calendar immediately with the
        // assumption that it will succeed on the emacs side.
        getCalInstance().createEvents([event]);
        //asyncSend({"command":"add-scheduled-event", data: event});
        // TODO: Can we also add this to the react-query collection so we don't get the flashign?
        useNewEvent.mutate({"command":"add-scheduled-event", data: event});
        return true;
    };

    const onBeforeDeleteEvent = (event) => {
        getCalInstance().deleteEvent(event.id, event.calendarId);
        asyncSend({"command":"remove-event", data: {id: event.id}});
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

            <InformationalModal message="You're in read-only mode. No changes can be made." visible={settings.readOnly}/>
            <Container breakpoint="fluid">

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
                    useFormPopup={true}
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
                    onBeforeDeleteEvent={onBeforeDeleteEvent}
                    onBeforeUpdateEvent={onBeforeUpdateSchedule}
                    onBeforeCreateEvent={onBeforeCreateEvent}
                />
            </Container>
            <Footer>
            <Container>
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
            </Container>
            </Footer>
            </Container>
        </div>
    );
}


function InformationalModal(props) {
    const [visible, setVisibility] = useState(props.visible);

    return (
        <Modal show={visible}>
            <Modal.Content>
            <Box>
            {props.message}
        <br />
            <Button.Group align="right">
            <Button color="light" onClick={() => setVisibility(false)}> Dismiss </Button>
            </Button.Group>
        </Box>
            </Modal.Content>
            </Modal>
    );
}


export default function App() {

  return (
    <QueryClientProvider client={queryClient}>
      <ReactCalendar />
    </QueryClientProvider>
  )
}
