import React from 'react'
import ReactDOM from 'react-dom/client'
import App from './App'

import {
  useQuery,
  useMutation,
  useQueryClient,
  QueryClient,
  QueryClientProvider,
} from 'react-query'


ReactDOM.createRoot(document.getElementById('root')!).render(
    <App />
)
