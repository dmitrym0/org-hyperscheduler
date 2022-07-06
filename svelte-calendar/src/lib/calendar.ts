import type { AgendaItem, CalendarEvent, CalendarConfig } from './types';

export function getUnixTimestampFromDate(date: Date): number {
	return date.getTime() / 1000;
}

export function transformAgendaToCalendarEvents(agenda: AgendaItem[], calendarConfigs: CalendarConfig[] = []): CalendarEvent[] {
	const schedule: CalendarEvent[] = [];
	let unscheduledTasks = 0;

	for (const agendaItem of agenda) {
		// skip elements that don't have startDate or endDate
		if (agendaItem.startDate === null) {
			unscheduledTasks++;
			continue;
		}

		let calendarItem: CalendarEvent = {
			id: agendaItem.ID,
			calendarId: agendaItem.calendarId || 'scheduled', // Use calendarId from Emacs or default
			title: agendaItem.ITEM.replaceAll(/\[\[.*:.*\]\[/ig, '').replaceAll(/\]\]/ig, ''),
			category: 'time',
			start: agendaItem.startDate,
			end: agendaItem.endDate!
		};

		// Set readonly based on calendar configuration
		const calendarConfig = calendarConfigs.find(config => config.id === calendarItem.calendarId);
		if (calendarConfig?.readOnly) {
			calendarItem.isReadOnly = calendarConfig.readOnly;
		}

                console.log(agendaItem);
		console.log(`Event "${agendaItem.ITEM}" assigned to calendar: ${calendarItem.calendarId}`);

		if (agendaItem.allDay === "true") {
			calendarItem.category = 'allday';
		}

		// Handle clocked items - check if they should be shown separately
		if (agendaItem.clockedList.length && agendaItem.showClockedSeparate) {
			for (const clockedItem of agendaItem.clockedList) {
				let clockedEntry: CalendarEvent = {
					id: `${agendaItem.ID}-clocked-${clockedItem.startDate}`,
					calendarId: "clocked",
					title: agendaItem.ITEM.replaceAll(/\[\[.*:.*\]\[/ig, '').replaceAll(/\]\]/ig, ''),
					category: 'time',
					start: clockedItem.startDate,
					end: clockedItem.endDate,
				};

				// Set readonly based on calendar configuration for clocked entries
				const clockedConfig = calendarConfigs.find(config => config.id === clockedEntry.calendarId);
				if (clockedConfig?.readOnly) {
					clockedEntry.isReadOnly = clockedConfig.readOnly;
				}
				schedule.push(clockedEntry);
			}
		}

		schedule.push(calendarItem);
	}

	console.log('There are ' + unscheduledTasks + ' unscheduled tasks.');
	return schedule;
}

// Default calendar configuration (fallback)
export const defaultCalendarConfig: CalendarConfig[] = [
	{
		id: 'scheduled',
		name: 'Scheduled Items',
		color: '#ffffff',
		bgColor: '#9e5fff',
		dragBgColor: '#9e5fff',
		borderColor: '#9e5fff'
	},
	{
		id: 'timestamped',
		name: 'Timestamped Items',
		color: '#000000',
		bgColor: '#00a9ff',
		dragBgColor: '#00a9ff',
		borderColor: '#00a9ff'
	},
	{
		id: 'done',
		name: 'Done Items',
		color: '#000000',
		bgColor: '#C0C0C0',
		dragBgColor: '#C0C0C0',
		borderColor: '#C0C0C0'
	},
	{
		id: 'work',
		name: 'Work',
		color: '#000000',
		bgColor: '#FFFF6e',
		dragBgColor: '#FFFF6e',
		borderColor: '#FFFF6e'
	},
	{
		id: 'clocked',
		name: 'Clocked Items',
		color: '#000000',
		bgColor: '#e2fee2',
		dragBgColor: '#e2fee2',
		borderColor: '#e2fee2'
	},
	{
		id: 'cancelled',
		name: 'Cancelled',
		color: '#000000',
		bgColor: '#FAA0A0',
		dragBgColor: '#FAA0A0',
		borderColor: '#FAA0A0'
	}
];
