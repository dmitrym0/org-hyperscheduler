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
  console.log(" websocket INIT INIT INIT");
  const websocket = new WebSocket("ws://127.0.0.1:44445");

  websocket.onopen = () => {
    console.log('connected')

  }


  websocket.onmessage = (event) => {
    console.log("ON MESSAGE");
    // const data = JSON.parse(event.data)
    // const queryKey = [...data.entity, data.id].filter(Boolean)
    // queryClient.invalidateQueries(queryKey)

  }

  return websocket;

}


const websocket = bindWebSocket()


// TODO:
// 1. on fetch agenda, de-register normal handler..
// 2. register an agenda specific handler
// 3. This agenda specific handler returns to useQuery??
// 4. Reregister the standard handler
//
// error checking in handlers?
const fetchAgenda = () => {
  return syncSend({ "command": "get-agenda" });
}

// a helper that turns websocket messaging into a synchronous affair.
const syncSend = (payload) => {
  const string_payload = JSON.stringify(payload);
  websocket.send(string_payload);
  var promise = new Promise(function(resolve, reject) {
    let syncResponseHandler = (event) => {
      console.log(`Response for ${string_payload}`);
      const data = JSON.parse(event.data);
      resolve(data);
      websocket.removeEventListener('message', syncResponseHandler);
    }
    websocket.addEventListener('message', syncResponseHandler);
  });
  return promise;
}





export const useAgenda = () => useQuery(["agenda"], fetchAgenda);




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
  const { isLoading, error, data } = useAgenda();

  if ( isLoading ) {
    return <div> Loading </div>
  }

  const containerRef = useRef(); // TODO: Magic, how does this work?
  const calendarRef = useRef(); // TODO: Magic, how does this work?

  console.log("Rendering MyCalendar " + calendarRef.current);

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
      calendarRef.current?.createSchedules(parseAgenda(data));
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
