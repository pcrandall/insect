import { appWindow } from '@tauri-apps/api/window'
document
  .getElementById('titlebar-minimize')
  .addEventListener('click', () => appWindow.minimize())
document
  .getElementById('titlebar-maximize')
  .addEventListener('click', async () => await appWindow.isMaximized() ? appWindow.unmaximmize() : appWindow.maximize())
document
  .getElementById('titlebar-close')
  .addEventListener('click', () => appWindow.close())
