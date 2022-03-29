var schedule = window.localStorage.getItem('schedule');

function setEventListener() {
  // $('.dropdown-menu a[role="menuitem"]').on('click', onClickMenu);
  $('#menu-navi').on('click', onClickNavi);
  // window.addEventListener('resize', resizeThrottled);
}

// from https://github.com/nhn/tui.calendar/blob/ac65d491c21b99ba18e179664c96089bd51216c7/examples/js/default.js
function getDataAction(target) {
  return target.dataset ? target.dataset.action : target.getAttribute('data-action');
}


// we need to re-render the calendar so that that time/hour marker gets updated.
function refreshCalendar() {
    setTimeout(refreshCalendar, 300000);
    cal.render();
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
    case 'change-day':
      cal.changeView('day', true);
      break;
    case 'change-week':
      cal.changeView('week', true);
      break;
    default:
      return;
  }

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
    useDetailPopup: true,
    taskView: false, 
    usageStatistics: false,
    isReadOnly: false,
      week: {
        narrowWeekend: true,
        startDayOfWeek: 1 // monday
      },
    scheduleView:  ['allday', 'time'],

  });

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


function getAgenda() {
    $("body").addClass("loading");
    if (schedule === null) {
        $("body").addClass("loading");
    } else {
        schedule = JSON.parse(schedule);
        console.log(`[hs] Loading schedule from localStorage, ${schedule.length} entries.`);
        cal.createSchedules(schedule);
    }
    console.log("[open] Connection established");
    console.log("Sending to server");
    socket.send(JSON.stringify({"command":"get-agenda"}));
}

socket.onopen = function(e) {
    getAgenda();
};

socket.onmessage = function(event) {
    for (let existingEvent of schedule) {
        cal.deleteSchedule(existingEvent.id, existingEvent.calendarId, false);
    }
    schedule = [];
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
            title: agendaItem["ITEM"].replaceAll(/\[\[.*\:.*\]\[/ig, '').replaceAll(/\]\]/ig, ''),
            category: 'time',
            start: agendaItem["startDate"],
            end: agendaItem["endDate"],
            isReadOnly: agendaItem["isReadOnly"],
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


    
    if (isReadOnly) {
        alert("Readonly mode; please see customized-group org-hyperscheduler");
        cal.setOptions({isReadOnly:true});
    $("body").removeClass("loading");
    cal.createSchedules(schedule);
    window.localStorage.setItem('schedule', JSON.stringify(schedule));
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
            e.startUnix = getUnixTimestampFromDate(e.start);
            e.endUnix =  getUnixTimestampFromDate(e.end);
            socket.send(JSON.stringify({"command":"add-scheduled-event", data: e}));
            getAgenda();
        },
        beforeDeleteSchedule: function (e) {
            console.log('beforeDeleteSchedule', e);
            calendar.deleteSchedule(e.schedule.id, e.schedule.calendarId);
        },
        afterRenderSchedule: function (e) {
            console.log('after render');
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


function isReadOnly() {
    return agenda.at(0).isReadOnly;
}

refreshCalendar();
