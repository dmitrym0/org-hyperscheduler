<script lang="ts">
	import { onMount } from 'svelte';
	import { agenda, isConnected, isLoading, lastSyncTime, syncedItemCount, calendarConfig } from '$lib/websocket';
	import { transformAgendaToCalendarEvents, getUnixTimestampFromDate } from '$lib/calendar';
	import { websocketManager } from '$lib/websocket';

	let calendarElement: HTMLElement;
	let calendar: any;
	let showSyncInfo = false;
	let currentDateRange = '';

	function formatSyncTime(date: Date | null): string {
		if (!date) return 'Never';
		return date.toLocaleTimeString();
	}

	function updateDateRange() {
		if (!calendar) return;

		const viewName = calendar.getViewName();
		const dateRangeStart = calendar.getDateRangeStart();
		const dateRangeEnd = calendar.getDateRangeEnd();

		const startDate = new Date(dateRangeStart.getTime());
		const endDate = new Date(dateRangeEnd.getTime());

		if (viewName === 'month') {
			if (startDate.getMonth() === endDate.getMonth() && startDate.getFullYear() === endDate.getFullYear()) {
				// Single month view
				currentDateRange = startDate.toLocaleDateString('en-US', {
					year: 'numeric',
					month: 'long'
				});
			} else {
				// Multiple months view
				currentDateRange = `${startDate.toLocaleDateString('en-US', {
					month: 'long',
					year: 'numeric'
				})} - ${endDate.toLocaleDateString('en-US', {
					month: 'long',
					year: 'numeric'
				})}`;
			}
		} else if (viewName === 'week') {
			if (startDate.getMonth() === endDate.getMonth()) {
				currentDateRange = `${startDate.toLocaleDateString('en-US', {
					month: 'long',
					day: 'numeric'
				})} - ${endDate.toLocaleDateString('en-US', {
					month: 'long',
					day: 'numeric',
					year: 'numeric'
				})}`;
			} else {
				currentDateRange = `${startDate.toLocaleDateString('en-US', {
					month: 'long',
					day: 'numeric'
				})} - ${endDate.toLocaleDateString('en-US', {
					month: 'long',
					day: 'numeric',
					year: 'numeric'
				})}`;
			}
		} else if (viewName === 'day') {
			currentDateRange = startDate.toLocaleDateString('en-US', {
				weekday: 'long',
				year: 'numeric',
				month: 'long',
				day: 'numeric'
			});
		}
	}

	onMount(async () => {
		// Load TUI Calendar scripts
		await loadScripts();
		initializeCalendar();
	});

	async function loadScripts() {
		const scripts = [
			'https://uicdn.toast.com/tui.code-snippet/v1.5.2/tui-code-snippet.min.js',
			'https://uicdn.toast.com/tui.time-picker/latest/tui-time-picker.min.js',
			'https://uicdn.toast.com/tui.date-picker/latest/tui-date-picker.min.js',
			'https://uicdn.toast.com/tui-calendar/latest/tui-calendar.js'
		];

		for (const src of scripts) {
			await new Promise((resolve) => {
				const script = document.createElement('script');
				script.src = src;
				script.onload = resolve;
				document.head.appendChild(script);
			});
		}
	}

	function initializeCalendar() {
		if (!window.tui) return;

		calendar = new window.tui.Calendar(calendarElement, {
			calendars: $calendarConfig.length > 0 ? $calendarConfig : [],
			defaultView: 'week',
			month: {
				daynames: ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'],
				startDayOfWeek: 1,
				narrowWeekend: true,
				visibleWeeksCount: 0,
				workweek: false
			},
			useCreationPopup: true,
			useDetailPopup: true,
			taskView: false,
			usageStatistics: false,
			isReadOnly: false,
			week: {
				narrowWeekend: true,
				startDayOfWeek: 1
			},
			scheduleView: ['allday', 'time'],
			template: {
				monthGridHeader: function(model) {
                                    let date = new Date(model.date);
                                    var template = '<span class="tui-full-calendar-weekday-grid-date">';
                                    template += '<span class="tui-full-calendar-weekday-grid-date-decorator">';
                                    template += '<span class="month-name">' +
                                                date.toLocaleDateString('en-US', { month: 'short', day: 'numeric', timeZone: 'UTC' }) +
                                                '</span>';
                                    template += '</span></span>';
                                    return template;				}
			}
		});

		// Set up calendar event handlers
		calendar.on('beforeUpdateSchedule', (event: any) => {
			const updated_schedule = event.schedule;
			const changes = event.changes;

			if (changes.start === undefined) {
				changes.start = updated_schedule.start;
			}

			const updateData = {
				id: updated_schedule.id,
				start: getUnixTimestampFromDate(changes.start),
				end: getUnixTimestampFromDate(changes.end)
			};

			websocketManager.updateEvent(updateData.id, updateData.start, updateData.end);
			calendar.updateSchedule(updated_schedule.id, updated_schedule.calendarId, changes);
		});

		calendar.on('beforeCreateSchedule', (e: any) => {
			e.startUnix = getUnixTimestampFromDate(e.start);
			e.endUnix = getUnixTimestampFromDate(e.end);
			websocketManager.addScheduledEvent(e);
			// Remove the immediate getAgenda() call - let the WebSocket response handle updates
		});

		calendar.on('beforeDeleteSchedule', (e: any) => {
			calendar.deleteSchedule(e.schedule.id, e.schedule.calendarId);
			websocketManager.removeEvent(e.schedule.id);
		});

		// Update date range initially
		updateDateRange();
	}

	// Reactive statement to update calendar when agenda changes
	let lastAgendaUpdate: string = '';
	$: if (calendar && $agenda) {
		// Prevent infinite loops by checking if agenda actually changed
		const currentAgendaString = JSON.stringify($agenda);
		if (currentAgendaString !== lastAgendaUpdate) {
			lastAgendaUpdate = currentAgendaString;

			console.log('Updating calendar with', $agenda.length, 'agenda items');
			
			// Clear existing schedules
			calendar.clear();

			// Transform and add new schedules
			const schedules = transformAgendaToCalendarEvents($agenda, $calendarConfig);
			calendar.createSchedules(schedules);

			// Check if readonly mode
			if ($agenda.length > 0 && $agenda[0].isReadOnly) {
				calendar.setOptions({ isReadOnly: true });
			}
		}
	}

	// Reactive statement to update calendar configuration when it changes
	$: if (calendar && $calendarConfig.length > 0) {
		console.log('Updating calendar configuration:', $calendarConfig);
		calendar.setCalendars($calendarConfig);
	}

	function handleNavigation(action: string) {
		if (!calendar) return;

		switch (action) {
			case 'move-prev':
				calendar.prev();
				updateDateRange();
				break;
			case 'move-next':
				calendar.next();
				updateDateRange();
				break;
			case 'move-today':
				calendar.today();
				updateDateRange();
				break;
			case 'change-day':
				calendar.changeView('day', true);
				updateDateRange();
				break;
			case 'change-week':
				calendar.changeView('week', true);
				updateDateRange();
				break;
			case 'change-month':
				calendar.changeView('month', true);
				updateDateRange();
				break;
		}
	}
</script>

<svelte:head>
	<title>Org Hyperscheduler</title>
</svelte:head>

<div class="container" class:loading={$isLoading}>
	<header class="calendar-header">
		<h1>{currentDateRange}</h1>
	</header>
	<div id="menu">
		<span id="menu-navi">
			<button type="button" class="btn" on:click={() => handleNavigation('move-today')}>
				Today
			</button>
			<button type="button" class="btn" on:click={() => handleNavigation('change-day')}>
				Day
			</button>
			<button type="button" class="btn" on:click={() => handleNavigation('change-week')}>
				Week
			</button>
			<button type="button" class="btn" on:click={() => handleNavigation('change-month')}>
				Month
			</button>
			<button type="button" class="btn" on:click={() => handleNavigation('move-prev')}>
				←
			</button>
			<button type="button" class="btn" on:click={() => handleNavigation('move-next')}>
				→
			</button>
			<button type="button" class="btn refresh-btn" on:click={() => websocketManager.getAgenda()} title="Refresh agenda data from Emacs" disabled={$isLoading}>
				{#if $isLoading}
					<span class="spinner">◐</span>
				{:else}
					↻
				{/if}
			</button>
		</span>
		<span
			class="connection-status"
			class:connected={$isConnected}
			role="status"
			on:mouseenter={() => showSyncInfo = true}
			on:mouseleave={() => showSyncInfo = false}
		>
			{$isConnected ? 'Connected' : 'Disconnected'}
			{#if showSyncInfo}
				<div class="sync-info-tooltip">
					<div>Last sync: {formatSyncTime($lastSyncTime)}</div>
					<div>Items synced: {$syncedItemCount}</div>
				</div>
			{/if}
		</span>
	</div>

	<div bind:this={calendarElement} id="calendar"></div>
</div>

<style>
	:global(html, body) {
		margin: 0;
		padding: 0;
		height: 100%;
		overflow: hidden;
	}

	.container {
		height: 100vh;
		width: 100vw;
		display: flex;
		flex-direction: column;
		overflow: hidden;
	}

	.calendar-header {
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		color: white;
		padding: 20px;
		text-align: center;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
	}

	.calendar-header h1 {
		margin: 0;
		font-size: 28px;
		font-weight: 300;
		letter-spacing: 1px;
	}

	#menu {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 10px;
		background: #f5f5f5;
		border-bottom: 1px solid #ddd;
		flex-shrink: 0;
	}

	#menu-navi {
		display: flex;
		gap: 10px;
	}

	.btn {
		padding: 8px 16px;
		border: 1px solid #ddd;
		background: white;
		border-radius: 4px;
		cursor: pointer;
		font-size: 14px;
	}

	.btn:hover {
		background: #f0f0f0;
	}

	.refresh-btn {
		margin-left: 10px;
		font-size: 16px;
		font-weight: bold;
	}

	.refresh-btn:hover:not(:disabled) {
		background: #e8f5e8;
		border-color: #51cf66;
	}

	.refresh-btn:disabled {
		opacity: 0.6;
		cursor: not-allowed;
	}

	.refresh-btn:disabled .spinner {
		opacity: 1;
		color: #007bff;
	}

	.spinner {
		width: 16px;
		height: 16px;
		color: white;
		border-radius: 50%;
		display: inline-flex;
		align-items: center;
		justify-content: center;
		animation: spin 1s linear infinite;
		font-weight: bold;
		flex-shrink: 0;
	}

	@keyframes spin {
		from { transform: rotate(0deg); }
		to { transform: rotate(360deg); }
	}

	.connection-status {
		font-size: 12px;
		padding: 4px 8px;
		border-radius: 4px;
		background: #ff6b6b;
		color: white;
	}

	.connection-status.connected {
		background: #51cf66;
	}

	.connection-status {
		position: relative;
		cursor: help;
	}

	.sync-info-tooltip {
		position: absolute;
		top: 100%;
		right: 0;
		margin-top: 5px;
		padding: 8px 12px;
		background: #333;
		color: white;
		border-radius: 4px;
		font-size: 11px;
		white-space: nowrap;
		z-index: 1000;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.2);
	}

	.sync-info-tooltip::before {
		content: '';
		position: absolute;
		bottom: 100%;
		right: 10px;
		border: 5px solid transparent;
		border-bottom-color: #333;
	}

	#calendar {
		flex: 1;
		min-height: 0;
		overflow: hidden;
	}

	.loading {
		opacity: 0.5;
		pointer-events: none;
	}

	:global(.month-name) {
		font-size: 10px;
		color: #666;
		font-weight: normal;
		display: block;
		margin-top: 2px;
	}

	:global(.tui-full-calendar-weekday-grid-line .tui-full-calendar-weekday-grid-date) {
		width: 60px !important;
	}

	:global(.tui-full-calendar-month-week-item .tui-full-calendar-today .tui-full-calendar-weekday-grid-date-decorator .month-name) {
		color: white !important;
		font-weight: bold !important;

		width: 45px !important;
	}

        :global(.tui-full-calendar-month-week-item .tui-full-calendar-today .tui-full-calendar-weekday-grid-date-decorator) {
		width: 45px !important;
	}



</style>
