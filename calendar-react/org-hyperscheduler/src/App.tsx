import * as React from "react";
import { useState, useEffect, useRef} from 'react'
import logo from './logo.svg'
import './App.css'

import Calendar from "tui-calendar";

import "tui-calendar/dist/tui-calendar.css";

import './index.css'


import { createWebStoragePersistor } from "react-query/createWebStoragePersistor-experimental"

import { persistQueryClient } from 'react-query/persistQueryClient-experimental'



import { ReactQueryDevtools } from "react-query/devtools";


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



const useNewEvent = (payload) => {
  const queryClient = useQueryClient()

  return useMutation(
    () => {
      const string_payload = JSON.stringify(payload);
      websocket.send(payload);
    },
    {
      onSuccess: () => {
        queryClient.invalidateQueries(['agenda'])
      },
    }
  )
}

const useAgenda = () => useQuery(["agenda"],
  fetchAgenda,
  {
    //staleTime: 30 * 1000,
    // placeholderData: () => {
    //   console.log("[rQuery] placeholder data");
    // },
    // initialData: () => {
    //   console.log("[rQuery] checking initial data");
    //   // Get the query state
    //   const state = queryClient.getQueryState(['agenda'])

    //   // // If the query exists and has data that is no older than 10 seconds...
    //   // if (state && Date.now() - state.dataUpdatedAt <= 10 * 1000) {
    //   //   // return the individual todo
    //   //   return state.data.find(d => d.id === todoId)
    //   //}

      // Otherwise, return undefined and let it fetch from a hard loading state!
    //},
  });

const calendars = [
    {
        id: '1',
        name: 'Scheduled Items',
        color: '#ffffff',
        bgColor: '#9e5fff',
        dragBgColor: '#9e5fff',
        borderColor: '#9e5fff'
    },
    {
        id: '2',
        name: 'Timestamped Items',
        color: '#000000',
        bgColor: '#00a9ff',
        dragBgColor: '#00a9ff',
        borderColor: '#00a9ff'
    },
];


const parseAgenda = (agenda) => {
  const schedule = [];
  if (agenda === undefined) {
    console.warn("Empty agenda!");
    return;
  }

  for (const agendaItem of agenda) {
    let calendarItem = {
      id: agendaItem["ID"],
      calendarId: 1,
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
      calendarItem.calendarId = 2;
    }
    schedule.push(calendarItem);
  }


  return schedule;
}


export function MyCalendar(props) {
  const containerRef = useRef(); // TODO: Magic, how does this work?
  const calendarRef = useRef(); // TODO: Magic, how does this work?


  const updateSchedule = (agenda) => {
    if (agenda !== undefined) {
      calendarRef.current?.clear();
      calendarRef.current?.createSchedules(agenda);
    }
  }



  const { isLoading, error, data } = useAgenda();

  if ( isLoading ) {
    return <div> Loading </div>
  } else {
    updateSchedule(parseAgenda(data["agenda"]));
  }

 console.log("- Rendering MyCalendar " + calendarRef.current);

  useEffect(() => {
    console.log("Creating calendar instance.");
    calendarRef.current = new Calendar(containerRef.current, {
      calendars: calendars,
      defaultView: 'week', // set 'month'
      month: {
        visibleWeeksCount: 2 // visible week count in monthly
      },

      useCreationPopup: true,
      useDetailPopup: true,
      taskView: false,
      usageStatistics: false,
      isReadOnly: false,
      week: {
        narrowWeekend: true,
        startDayOfWeek: 1 // monday
      },
      scheduleView: ['allday', 'time'],


    });


    if (calendarRef.current) {
      updateSchedule(parseAgenda(data["agenda"]));
    }

    return () => {
      if (calendarRef.current) {
        console.log("Destroying calendar.");
        calendarRef.current.destroy();
      }
    };
  }, []);



  return (
    <div ref={containerRef} />
  );
}





export default function App() {

  return (
    <QueryClientProvider client={queryClient}>
      <MyCalendar />
      <ReactQueryDevtools  />
    </QueryClientProvider>
  )
}
