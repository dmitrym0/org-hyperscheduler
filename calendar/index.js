function setEventListener() {
  // $('.dropdown-menu a[role="menuitem"]').on('click', onClickMenu);
  $('#menu-navi').on('click', onClickNavi);
  // window.addEventListener('resize', resizeThrottled);
}

// from https://github.com/nhn/tui.calendar/blob/ac65d491c21b99ba18e179664c96089bd51216c7/examples/js/default.js
function getDataAction(target) {
  return target.dataset ? target.dataset.action : target.getAttribute('data-action');
}


function onClickNavi(e) {
  var action = getDataAction(e.target);

  switch (action) {
    case 'move-prev':
      cal.prev();
      break;
    case 'move-next':
      cal.next();
      break;
    case 'move-today':
      cal.today();
      break;
    default:
      return;
  }

  setRenderRangeText();
  setSchedules();
}

cal = new tui.Calendar('#calendar', {
    calendars: [
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
    ],

    defaultView: 'week', // set 'month'
    month: {
      visibleWeeksCount: 2 // visible week count in monthly
    },
    
    useCreationPopup: true,
    useDetailPopup: true
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

calendar = cal;

cal.on('beforeUpdateSchedule', function(event) {
    updated_schedule = event.schedule;
    changes = event.changes;
    if (changes.start === undefined) {
        changes.start = updated_schedule.start;
    }
        

    console.log(`schedule:` + schedule + `changes:` + changes);

    console.log(`Time changed to ${getUnixTimestampFromDate(changes.end)}`);


    update_event_data = {id: updated_schedule.id, start: getUnixTimestampFromDate(changes.start), end: getUnixTimestampFromDate(changes.end)};
    
    socket.send(JSON.stringify({"command": "update-event", "data":update_event_data}));
    
    cal.updateSchedule(updated_schedule.id, updated_schedule.calendarId, changes);
});


function getUnixTimestampFromDate(date) {
    return date.getTime() / 1000;
}


let socket = new WebSocket("ws://127.0.0.1:44445");

socket.onopen = function(e) {
    $("body").addClass("loading");
    console.log("[open] Connection established");
    console.log("Sending to server");
    socket.send(JSON.stringify({"command":"get-agenda"}));
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
    $("body").removeClass("loading");
};


calendar.on({
        clickMore: function (e) {
            console.log('clickMore', e);
        },
        clickSchedule: function (e) {
            console.log('clickSchedule', e);
            //cal.openCreationPopup(e.schedule);
        },
        clickDayname: function (date) {
            console.log('clickDayname', date);
        },
        beforeCreateSchedule: function (e) {
            console.log('beforeCreateSchedule', e);
            socket.send(JSON.stringify({"command":"add-scheduled-event", data: e}));
        },
        beforeDeleteSchedule: function (e) {
            console.log('beforeDeleteSchedule', e);
            calendar.deleteSchedule(e.schedule.id, e.schedule.calendarId);
        },
        afterRenderSchedule: function (e) {
            // const schedule = e.schedule;
            // let element = calendar.getElement(schedule.id, schedule.calendarId);
            // console.log('afterRenderSchedule', element);
        }
});

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

setEventListener();
