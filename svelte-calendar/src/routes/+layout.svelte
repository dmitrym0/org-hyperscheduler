<script lang="ts">
	import { onMount } from 'svelte';
	import { websocketManager } from '$lib/websocket';

	onMount(() => {
		websocketManager.connect();
	});
</script>

<svelte:head>
	<link rel="stylesheet" type="text/css" href="https://uicdn.toast.com/tui-calendar/latest/tui-calendar.css" />
	<link rel="stylesheet" type="text/css" href="https://uicdn.toast.com/tui.date-picker/latest/tui-date-picker.css" />
	<link rel="stylesheet" type="text/css" href="https://uicdn.toast.com/tui.time-picker/latest/tui-time-picker.css" />
</svelte:head>

<main>
	<slot />
</main>

<style>
	:global(html, body) {
		margin: 0;
		padding: 0;
		height: 100%;
		width: 100%;
		font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
	}

	:global(.loading) {
		opacity: 0.5;
		pointer-events: none;
	}

	main {
		height: 100vh;
		width: 100vw;
		display: flex;
		flex-direction: column;
	}
</style>
