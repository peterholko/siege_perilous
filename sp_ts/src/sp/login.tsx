import React, { Component } from "react";
import Game from "./Game"
import UI from "./UI";
import {LOGIN_ATTEMPT} from './store/gameReducer'
import store from "./store";
import { Global } from "./global";
import { Network } from "./network";
import { NetworkEvent } from "./networkEvent";
import styles from "./app.css"


export default class LoginControl extends React.Component<any, any> {
  constructor(props) {
    super(props);

    this.state = {
      isLoggedIn: false,
      username: '',
      password: ''
    };

    this.handleLoginClick = this.handleLoginClick.bind(this);
    this.handleUsernameChange = this.handleUsernameChange.bind(this);
    this.handlePasswordChange = this.handlePasswordChange.bind(this);

    this.handleLoggedIn = this.handleLoggedIn.bind(this);

    Global.gameEmitter.on(NetworkEvent.LOGGED_IN, this.handleLoggedIn, this);
  }

  handleLoginClick() {
    store.dispatch({type: LOGIN_ATTEMPT});
    Network.sendLogin(this.state.username, this.state.password);
  }

  handleUsernameChange(event) {
    this.setState({username: event.target.value});
  }

  handlePasswordChange(event) {
    this.setState({password: event.target.value});
  }

  handleLoggedIn() {
    this.setState({isLoggedIn: true});
  }

  render() {
    const isLoggedIn = this.state.isLoggedIn;

    return (
      <div>
        {isLoggedIn ? (
            <div id="container" className={styles.container}>
              <UI />
              <Game />
            </div>
          ) : (
            <div>
              Username: <input type='text' 
                               id="username" 
                               value={this.state.username} 
                               onChange={this.handleUsernameChange} />
              Password: <input type='password' 
                               id="password" 
                               value={this.state.password} 
                               onChange={this.handlePasswordChange}/>
              <button onClick={this.handleLoginClick}>
                Login
              </button>
            </div>
          )
        }    
      </div>
    );
  }
}
