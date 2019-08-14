import React, { Component } from "react";
import { Provider } from "react-redux";

import store from "./store";
import LoginControl from "./login";
import { Network } from "./network";
import { Global } from "./global";

Global.socket = new Network();
Global.gameEmitter = new Phaser.Events.EventEmitter();
Global.uiEmitter = new Phaser.Events.EventEmitter();

class App extends Component {
  render() {
    return (
      <Provider store={store}>
        <div>
          <LoginControl />
        </div>
      </Provider>
    );
  }
}

export default App;