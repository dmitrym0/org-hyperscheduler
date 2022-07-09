import * as React from "react";
import { useState, useEffect} from 'react'
import logo from './logo.svg'
import './App.css'

import Calendar from '@toast-ui/react-calendar';
import '@toast-ui/calendar/dist/toastui-calendar.min.css';
import './index.css'

import {
  useQuery,
  useMutation,
  useQueryClient,
  QueryClient,
  QueryClientProvider,
} from 'react-query'


import { ReactQueryDevtools } from "react-query/devtools";


const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      staleTime: Infinity
    }
  }
});

const bindWebSocket = () => {
  console.log(" websocket INIT INIT INIT");
  const websocket = new WebSocket("ws://127.0.0.1:44445");

  websocket.onopen = () => {
    console.log('connected')
    websocket.send(JSON.stringify({"command":"get-agenda"}));
  }


  websocket.onmessage = (event) => {
    console.log("ON MESSAGE");
    // const data = JSON.parse(event.data)
    // const queryKey = [...data.entity, data.id].filter(Boolean)
    // queryClient.invalidateQueries(queryKey)

    const agenda = JSON.parse(event.data);
    console.log(`- ${agenda.length} items in agenda.`);
  }

  return websocket;

}


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

export function MyCalendar(props) {

  const [websocket, setSocket] = useState(() => {
    return bindWebSocket();
  });

  // useEffect(() => {
  //   console.log(`websocket.readyState = ${websocket.readyState}`);
  //   if (websocket.readyState == websocket.OPEN) {
  //     websocket.send(JSON.stringify({"command":"get-agenda"}));
  //   }
//  },[websocket.readyState])

  return (
    <div>
      <Calendar
        useCreationPopup={true}
        useDetailPopup={true}
        taskView={false}
        usageStatistics={false}
        isReadOnly={false}
        week={{
          narrowWeekend: true,
          startDayOfWeek: 1, // monday
        }}
        scheduleView={['time']}
      />
    </div>
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
