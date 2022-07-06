export interface AgendaItem {
	ID: string;
	ITEM: string;
	startDate: string | null;
	endDate: string | null;
	allDay?: string;
	SCHEDULED?: string;
	'CALENDAR-ID'?: string;
	TODO?: string;
	clockedList: ClockItem[];
	isReadOnly?: boolean;
	calendarId?: string;
	showClockedSeparate?: boolean;
}

export interface ClockItem {
	startDate: string;
	endDate: string;
}

export interface CalendarEvent {
	id: string;
	calendarId: string;
	title: string;
	category: 'time' | 'allday';
	start: string;
	end: string;
	isReadOnly?: boolean;
}

export interface CalendarConfig {
	id: string;
	name: string;
	color: string;
	bgColor: string;
	borderColor: string;
	dragBgColor: string;
	readOnly?: boolean;
}

export interface WebSocketMessage {
	command: string;
	data?: any;
	agenda?: AgendaItem[];
	entry?: AgendaItem;
	calendarConfig?: CalendarConfig[];
}
