import { writable } from 'svelte/store';
import type { WebSocketMessage, AgendaItem, CalendarConfig } from './types';

export const agenda = writable<AgendaItem[]>([]);
export const isConnected = writable(false);
export const isLoading = writable(false);
export const lastSyncTime = writable<Date | null>(null);
export const syncedItemCount = writable<number>(0);
export const calendarConfig = writable<CalendarConfig[]>([]);

class WebSocketManager {
	private socket: WebSocket | null = null;
	private reconnectAttempts = 0;
	private maxReconnectAttempts = 5;
	private reconnectInterval: number | null = null;
	private isLoadingAgenda = false;
	private hasLoadedInitialAgenda = false;

	connect() {
		try {
			this.socket = new WebSocket("ws://127.0.0.1:44445");
			
			this.socket.onopen = () => {
				console.log('WebSocket connected');
				isConnected.set(true);
				this.reconnectAttempts = 0;
				
				// Clear reconnection interval on successful connection
				if (this.reconnectInterval) {
					clearInterval(this.reconnectInterval);
					this.reconnectInterval = null;
				}
				
				// Get settings and agenda on initial connection or reconnection
				this.getSettings();
				this.getAgenda();
			};

			this.socket.onmessage = (event) => {
				const message: WebSocketMessage = JSON.parse(event.data);
				
				if (message.command === 'invalidate') {
					console.log('Invalidating cache');
					// Don't immediately call getAgenda() - let user manually refresh if needed
					// or implement a debounced refresh mechanism
					return;
				}

				if (message.command === 'update-single-entry' && message.entry) {
					console.log('Updating single entry:', message.entry.ID);
					agenda.update(currentAgenda => {
						const updatedAgenda = currentAgenda.filter(item => item.ID !== message.entry!.ID);
						updatedAgenda.push(message.entry!);
						
						// Cache updated agenda to localStorage
						localStorage.setItem('agenda', JSON.stringify(updatedAgenda));
						syncedItemCount.set(updatedAgenda.length);
						lastSyncTime.set(new Date());
						
						return updatedAgenda;
					});
					return;
				}

				if (message.command === 'update-calendar-config' && message.data) {
					console.log('Updating calendar configuration');
					calendarConfig.set(message.data);
					localStorage.setItem('calendarConfig', JSON.stringify(message.data));
					return;
				}

				if (message.command === 'update-settings' && message.data?.calendarConfig) {
					console.log('Updating calendar configuration from settings');
					calendarConfig.set(message.data.calendarConfig);
					localStorage.setItem('calendarConfig', JSON.stringify(message.data.calendarConfig));
				}

				if (message.agenda) {
					agenda.set(message.agenda);
					isLoading.set(false);
					this.isLoadingAgenda = false;
					this.hasLoadedInitialAgenda = true;
					lastSyncTime.set(new Date());
					syncedItemCount.set(message.agenda.length);
					
					// Cache to localStorage
					localStorage.setItem('agenda', JSON.stringify(message.agenda));
				}

				if (message.calendarConfig) {
					calendarConfig.set(message.calendarConfig);
					localStorage.setItem('calendarConfig', JSON.stringify(message.calendarConfig));
				}
			};

			this.socket.onclose = () => {
				console.log('WebSocket disconnected');
				isConnected.set(false);
				this.hasLoadedInitialAgenda = false; // Reset flag so agenda loads on reconnection
				this.loadFromCache();
				this.startReconnectionLoop();
			};

			this.socket.onerror = (error) => {
				console.error('WebSocket error:', error);
				isConnected.set(false);
			};

		} catch (error) {
			console.error('Failed to connect to WebSocket:', error);
			this.loadFromCache();
		}
	}

	private startReconnectionLoop() {
		// Clear any existing reconnection interval
		if (this.reconnectInterval) {
			clearInterval(this.reconnectInterval);
		}
		
		// Set up interval to try every 5 seconds (no immediate attempt)
		this.reconnectInterval = window.setInterval(() => {
			if (!this.socket || this.socket.readyState === WebSocket.CLOSED) {
				this.attemptReconnect();
			} else if (this.socket.readyState === WebSocket.OPEN) {
				// Successfully connected, clear the interval
				if (this.reconnectInterval) {
					clearInterval(this.reconnectInterval);
					this.reconnectInterval = null;
				}
			}
		}, 5000);
	}

	private attemptReconnect() {
		this.reconnectAttempts++;
		console.log(`Reconnection attempt ${this.reconnectAttempts}`);
		this.connect();
	}

	private loadFromCache() {
		const cached = localStorage.getItem('agenda');
		if (cached) {
			const cachedAgenda = JSON.parse(cached);
			agenda.set(cachedAgenda);
			syncedItemCount.set(cachedAgenda.length);
		}

		const cachedConfig = localStorage.getItem('calendarConfig');
		if (cachedConfig) {
			const cachedCalendarConfig = JSON.parse(cachedConfig);
			calendarConfig.set(cachedCalendarConfig);
		}
	}

	getAgenda() {
		if (this.socket?.readyState === WebSocket.OPEN && !this.isLoadingAgenda) {
			this.isLoadingAgenda = true;
			isLoading.set(true);
			this.socket.send(JSON.stringify({ command: "get-agenda" }));
		}
	}

	getSettings() {
		if (this.socket?.readyState === WebSocket.OPEN) {
			this.socket.send(JSON.stringify({ command: "get-settings" }));
		}
	}

	updateEvent(id: string, start: number, end: number) {
		if (this.socket?.readyState === WebSocket.OPEN) {
			const updateData = { id, start, end };
			this.socket.send(JSON.stringify({ 
				command: "update-event", 
				data: updateData 
			}));
		}
	}

	addScheduledEvent(eventData: any) {
		if (this.socket?.readyState === WebSocket.OPEN) {
			this.socket.send(JSON.stringify({ 
				command: "add-scheduled-event", 
				data: eventData 
			}));
		}
	}

	removeEvent(id: string) {
		if (this.socket?.readyState === WebSocket.OPEN) {
			this.socket.send(JSON.stringify({ 
				command: "remove-event", 
				data: { id } 
			}));
		}
	}
}

export const websocketManager = new WebSocketManager();
