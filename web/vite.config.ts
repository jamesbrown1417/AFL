import { defineConfig, loadEnv } from 'vite'
import react from '@vitejs/plugin-react'

// https://vite.dev/config/
export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), '')
  const proxyBearerToken = env.AFL_PROXY_BEARER_TOKEN
  const tailscaleLogin = env.AFL_PROXY_TAILSCALE_USER_LOGIN
  const tailscaleHeader = env.AFL_PROXY_TAILSCALE_USER_HEADER || 'Tailscale-User-Login'

  return {
    plugins: [react()],
    server: {
      proxy: {
        '/api': {
          target: env.VITE_BACKEND_PROXY_TARGET || 'http://127.0.0.1:8000',
          changeOrigin: true,
          configure(proxy) {
            proxy.on('proxyReq', (proxyReq) => {
              if (proxyBearerToken) {
                proxyReq.setHeader('Authorization', `Bearer ${proxyBearerToken}`)
              }
              if (tailscaleLogin) {
                proxyReq.setHeader(tailscaleHeader, tailscaleLogin)
              }
            })
          },
        },
      },
    },
  }
})
