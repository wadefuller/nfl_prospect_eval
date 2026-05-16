import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'

export default defineConfig({
  base: '/nfl_prospect_eval/',
  plugins: [react(), tailwindcss()],
})
