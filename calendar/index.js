/*

 index.js - companion to the org-hyperscheduler.el

 Copyright © 2022 Dmitry Markushevich

 Author: Dmitry Markushevich <dmitrym@gmail.com>
 Keywords: org-mode, calendar
 URL: https://github.com/dmitrym0/org-hyperscheduler

 This file is NOT part of GNU Emacs.

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 3, or (at your option)
 any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with GNU Emacs; see the file COPYING.  If not, write to the
 Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 Boston, MA 02110-1301, USA.


 Commentary:

 This is the javascript code that talks to Emacs via websockets to render agenda.

 More info https://github.com/dmitrym0/org-hyperscheduler

*/
// get the schedule from local storage so we can display something immediately.
let schedule = window.localStorage.getItem('schedule');
let agenda;

// set readonly mode
function setReadonly() {
  calendar.setOptions({isReadOnly:true});
}
function setEventListener() {
  $('#menu-navi').on('click', onClickNavi);
}

// from https://github.com/nhn/tui.calendar/blob/ac65d491c21b99ba18e179664c96089bd51216c7/examples/js/default.js
function getDataAction(target) {
  return target.dataset ? target.dataset.action : target.getAttribute('data-action');
}


// we need to re-render the calendar so that that time/hour marker gets updated.
function refreshCalendar() {
    setTimeout(refreshCalendar, 300000);
    calendar.render();
}

// handler for day/week/month views, as well next/previous.
function onClickNavi(e) {
  var action = getDataAction(e.target);

  switch (action) {
    case 'move-prev':
      calendar.prev();
      break;
    case 'move-next':
      calendar.next();
      break;
    case 'move-today':
      calendar.today();
      break;
    case 'change-day':
      calendar.changeView('day', true);
      break;
    case 'change-week':
      calendar.changeView('week', true);
      break;
    default:
      return;
  }

}

// initialize the calendar object.
// current support two calendars:
// - scheduled items (basically anything with a SCHEDULED: property)
// - time stamped items (in practice this comes from org-gcal)
let calendar = new tui.Calendar('#calendar', {
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
        {
            id: '3',
            name: 'Done Items',
            color: '#000000',
            bgColor: '#C0C0C0',
            dragBgColor: '#00a9ff',
            borderColor: '#00a9ff'
        },
        {
            id: '4',
            name: 'Work',
            color: '#000000',
            bgColor: '#FFFF6e',
            dragBgColor: '#00a9ff',
            borderColor: '#00a9ff'
        },
        {
            id: '5',
            name: 'Clocked Items',
            color: '#000000',
            bgColor: '#e2fee2',
            dragBgColor: '#00a9ff',
            borderColor: '#00a9ff'
        },
        {
            id: '6',
            name: 'Cancelled',
            color: '#000000',
            bgColor: '#FAA0A0',
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

// update calendar entry
calendar.on('beforeUpdateSchedule', function(event) {
    let updated_schedule = event.schedule;
    let changes = event.changes;
    if (changes.start === undefined) {
        changes.start = updated_schedule.start;
    }

    console.log(`schedule:` + schedule + `changes:` + changes);

    console.log(`Time changed to ${getUnixTimestampFromDate(changes.end)}`);

    let update_event_data = {id: updated_schedule.id, start: getUnixTimestampFromDate(changes.start), end: getUnixTimestampFromDate(changes.end)};
    socket.send(JSON.stringify({"command": "update-event", "data":update_event_data}));
    calendar.updateSchedule(updated_schedule.id, updated_schedule.calendarId, changes);
});


// convert javascript date to unix time stamp because that's easier to pass around.
function getUnixTimestampFromDate(date) {
    return date.getTime() / 1000;
}



// fetch agenda
// TODO: it would be nice if we could start working with cache while loading from emacs.
function getAgenda() {
    // expect this set from localStorage.
    schedule = JSON.parse(schedule);
    calendar.createSchedules(schedule);
    fetchNewAgenda();
}


function fetchNewAgenda()
{
    $("body").addClass("loading");
    socket.send(JSON.stringify({"command":"get-agenda"}));
}

// calendar call backs
calendar.on({
        clickMore: function (e) {
            console.log('clickMore', e);
        },
        clickSchedule: function (e) {
            console.log('clickSchedule', e);
            //calendar.openCreationPopup(e.schedule);
        },
        clickDayname: function (date) {
            console.log('clickDayname', date);
        },
        beforeCreateSchedule: function (e) {
            console.log('beforeCreateSchedule', e);
            e.startUnix = getUnixTimestampFromDate(e.start);
            e.endUnix =  getUnixTimestampFromDate(e.end);
            socket.send(JSON.stringify({"command":"add-scheduled-event", data: e}));
            fetchNewAgenda();
        },
        beforeDeleteSchedule: function (e) {
            console.log('beforeDeleteSchedule', e);
            calendar.deleteSchedule(e.schedule.id, e.schedule.calendarId);
            socket.send(JSON.stringify({"command":"remove-event", data: {id: e.schedule.id}}));
            return true;
        },
        afterRenderSchedule: function (e) {
            console.log('after render', e);
            // const schedule = e.schedule;
            // let element = calendar.getElement(schedule.id, schedule.calendarId);
            // console.log('afterRenderSchedule', element);
        }
});



// are we in readonly mode?
function isReadOnly() {
    // TODO: this will fail for new users with no agenda, and read only mode disabled.
    return agenda.length === 0 || agenda.at(0).isReadOnly;
}


// -- networking stuff ----
let socket = new WebSocket("ws://127.0.0.1:44445");


// callback for when the connection is established
socket.onopen = function() {
    getAgenda();
};


// callback for when we get data
socket.onmessage = function(event) {
    debugger
    // TODO: what?
    if (schedule !== null) {
        for (let existingEvent of schedule) {
            calendar.deleteSchedule(existingEvent.id, existingEvent.calendarId, false);
        }
    }

    if (JSON.parse(event.data).command === 'invalidate') {
        console.log('Invalidating cache');
        schedule = [];
        fetchNewAgenda();
        return;
    }

    schedule = [];
    console.log(`[message] Data received from server: ${event.data}`);
    agenda = JSON.parse(event.data).agenda;
    console.log(`${agenda?.length} items in agenda.`);
    schedule = [];

    let unscheduledTasks = 0;

    for (const agendaItem of agenda) {
        // skip elements that don't have startDate or endDate
        if (agendaItem["startDate"] === null ) {
            unscheduledTasks++;
            continue;
        }


        let calendarItem = {
            id:  agendaItem["ID"],
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

        if (agendaItem["CALENDAR-ID"] === 'dmitrym@gmail.com') {
        }

        if (agendaItem["CALENDAR-ID"] === 'dmitry.markushevich@varsitytutors.com') {
            calendarItem.calendarId = 4;
        }



        if (agendaItem["TODO"] === "DONE") {
            calendarItem.calendarId = 3;
            calendarItem.isReadOnly = true;
        }

        if (agendaItem["TODO"] === "CANCELLED") {
            calendarItem.calendarId = 6;
            calendarItem.isReadOnly = true;
        }



        if (agendaItem["clockedList"].length) {
            for (const clockedItem of agendaItem["clockedList"]) {
                let clockedEntry = {
                    id: agendaItem["ID"],
                    calendarId: "5",
                    title: agendaItem["ITEM"].replaceAll(/\[\[.*:.*\]\[/ig, '').replaceAll(/\]\]/ig, ''),
                    category: 'time',
                    start: clockedItem["startDate"],
                    end: clockedItem["endDate"],
                    isReadOnly: true
                };
                schedule.push(clockedEntry);
            }
        }


        schedule.push(calendarItem);
    }

    console.log('There are ' + unscheduledTasks + ' unscheduled tasks.');


    if (isReadOnly()) {
        alert("Readonly mode; please see customized-group org-hyperscheduler");

    }

    $("body").removeClass("loading");

    calendar.createSchedules(schedule);

    window.localStorage.setItem('schedule', JSON.stringify(schedule));
};

socket.onclose = function(event) {
    alert("Lost connection to Emacs. Going into readonly-mode.");
    setReadonly();
};

socket.onerror = function(error) {
    alert("Connection error. Going into readonly-mode.");
    // this gets picked up from local storage. probably should unify this with getAgenda()
    calendar.createSchedules(JSON.parse(schedule));
    setReadonly();
};


setEventListener();
refreshCalendar();
