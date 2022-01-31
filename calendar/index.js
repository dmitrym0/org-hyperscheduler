cal = new tui.Calendar('#calendar', {
    calendars: [
    {
      id: '1',
      name: 'My Calendar',
      color: '#ffffff',
      bgColor: '#9e5fff',
      dragBgColor: '#9e5fff',
      borderColor: '#9e5fff'
    },
    {
      id: '2',
      name: 'Company',
      color: '#000000',
      bgColor: '#00a9ff',
      dragBgColor: '#00a9ff',
      borderColor: '#00a9ff'
    },
    ],

    defaultView: 'week', // set 'month'
    month: {
      visibleWeeksCount: 2 // visible week count in monthly
    }
  });

/*
cal.createSchedules([
    {
        id: '1',
        calendarId: '1',
        title: 'my schedule',
        category: 'time',
        dueDateClass: '',
        start: '2022-01-18T22:30:00+09:00',
        end: '2022-01-19T02:30:00+09:00'
    },
    {
        id: '2',
        calendarId: '1',
        title: 'second schedule',
        category: 'time',
        dueDateClass: '',
        start: '2022-01-18T12:00:00-08:00',
        end: '2022-01-18T13:00:00-08:00'
    }
]);
*/

cal.on('beforeUpdateSchedule', function(event) {
    updated_schedule = event.schedule;
    changes = event.changes;

    console.log(`schedule:` + schedule + `changes:` + changes);

    console.log(`Time changed to ${getUnixTimestampFromDate(changes.end)}`);

    cal.updateSchedule(updated_schedule.id, updated_schedule.calendarId, changes);
});


function getUnixTimestampFromDate(date) {
    return date.getTime() / 1000;
}


let socket = new WebSocket("ws://127.0.0.1:44445");

socket.onopen = function(e) {
    console.log("[open] Connection established");
    console.log("Sending to server");
    socket.send(JSON.stringify({"command":"getAgenda"}));
};

socket.onmessage = function(event) {
    console.log(`[message] Data received from server: ${event.data}`);
    agenda = JSON.parse(event.data);
    console.log(`${agenda.length} items in agenda.`);
    let calId = 0;
    schedule = [];
    for (const agendaItem of agenda) {
        let calendarItem = {
            id:  agendaItem["ID"],
            calendarId: 1,
            dueDateClass: '',
            title: agendaItem["ITEM"].replaceAll(/\[\[.*\:.*\]\[/ig, ''),
            category: 'time',
            start: agendaItem["startDate"],
            end: agendaItem["endDate"],
            // isAllDay: agendaItem["allDay"]
        };


        if (agendaItem["allDay"] === "true") {
            calendarItem.category = 'allday';
        }
        calId = calId + 1;

        if (agendaItem["SCHEDULED"] === undefined) {
            calendarItem.calendarId = 2;
        }

        schedule.push(calendarItem);
    }


    
    cal.createSchedules(schedule);
};

socket.onclose = function(event) {
  if (event.wasClean) {
    alert(`[close] Connection closed cleanly, code=${event.code} reason=${event.reason}`);
  } else {
    // e.g. server process killed or network down
    // event.code is usually 1006 in this case
    alert('[close] Connection died');
  }
};

socket.onerror = function(error) {
  alert(`[error] ${error.message}`);
};
