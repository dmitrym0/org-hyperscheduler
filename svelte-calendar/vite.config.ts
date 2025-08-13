import { svelte } from '@sveltejs/vite-plugin-svelte';
import { defineConfig } from 'vite';

export default defineConfig(({ command }) => {
	const config = {
		plugins: [svelte()],
		base: './',
		build: {
			rollupOptions: {
				input: command === 'build' ? 'src/main.js' : 'index.html',
				output: {
					entryFileNames: 'app.js',
					chunkFileNames: 'app.js',
					assetFileNames: (assetInfo) => {
						if (assetInfo.name && assetInfo.name.endsWith('.css')) {
							return 'app.css';
						}
						return '[name].[ext]';
					},
					format: command === 'build' ? 'iife' : 'es',
					name: command === 'build' ? 'OrgHyperscheduler' : undefined
				}
			},
			cssCodeSplit: false
		}
	};

	return config;
});
