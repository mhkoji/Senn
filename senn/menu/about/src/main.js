const electron = require('electron')

function createWindow () {
  // Create the browser window.
  const win = new electron.BrowserWindow({
    width: 464,
    height: 200,
    resizable: false,
    webPreferences: {
      nodeIntegration: true
    }
  });

  win.setMenu(null);

  // and load the index.html of the app.
  win.loadFile('src/index.html')

  // Open the DevTools.
  // win.webContents.openDevTools()
}

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
electron.app.whenReady().then(createWindow);

// Quit when all windows are closed.
electron.app.on('window-all-closed', () => {
  // On macOS it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (process.platform !== 'darwin') {
    electron.app.quit()
  }
});

electron.app.on('activate', () => {
  // On macOS it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (BrowserWindow.getAllWindows().length === 0) {
    createWindow()
  }
});
