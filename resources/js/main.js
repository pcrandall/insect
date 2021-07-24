window.myApp = {
    setTray: () => {
        if(NL_MODE != "window") {
            console.log("INFO: Tray menu is only available in the window mode.");
            return;
        }
        let tray = {
            icon: "/resources/icons/trayIcon.png",
            menuItems: [
                {id: "VERSION", text: "Get version"},
                {id: "SEP", text: "-"},
                {id: "QUIT", text: "Quit"}
            ]
        };
        Neutralino.os.setTray(tray);
    }
};

Neutralino.events = {
    onTrayMenuItemClicked: (menuItem) => {
        switch(menuItem.id) {
            case "VERSION":
                Neutralino.os.showMessageBox({
                    type: "INFO",
                    title: "Version information",
                    content: `Neutralinojs server: v${NL_VERSION} | Neutralinojs client: v${NL_CVERSION}`
                });
                break;
            case "QUIT":
                Neutralino.app.exit();
                break;
        }
    }
};

Neutralino.init();
window.myApp.setTray();
