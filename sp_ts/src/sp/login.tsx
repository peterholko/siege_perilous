import React, { Component, ChangeEvent } from "react";
import Game from "./game"
import UI from "./ui";
import {LOGIN_ATTEMPT} from './store/gameReducer'
import store from "./store";
import { Global } from "./global";
import { Network } from "./network";
import { NetworkEvent } from "./networkEvent";
import "./login.css"
import logo from "art/perilous_logo.png";

export default class LoginControl extends React.Component<any, any> {
  constructor(props) {
    super(props);

    this.state = {
      isLoggedIn: false,
      username: 'Username',
      password: 'Password'
    };

    this.handleLoginClick = this.handleLoginClick.bind(this);
    this.handleUsernameChange = this.handleUsernameChange.bind(this);
    this.handlePasswordChange = this.handlePasswordChange.bind(this);

    this.handleUsernameBlur = this.handleUsernameBlur.bind(this);
    this.handleUsernameFocus = this.handleUsernameFocus.bind(this);
    this.handlePasswordBlur = this.handlePasswordBlur.bind(this);
    this.handlePasswordFocus = this.handlePasswordFocus.bind(this);

    this.handleLoggedIn = this.handleLoggedIn.bind(this);

    Global.gameEmitter.on(NetworkEvent.LOGGED_IN, this.handleLoggedIn, this);
  }

  handleLoginClick(event) {
    store.dispatch({type: LOGIN_ATTEMPT});
    Network.sendLogin(this.state.username, this.state.password);

    event.preventDefault();
  }

  handleUsernameChange(event) {
    this.setState({username: event.target.value});
  }

  handlePasswordChange(event) {
    this.setState({password: event.target.value});
  }

  handleUsernameBlur(event) {
    if(event.target.value == '') { 
      this.setState({username: 'Username'});
    }
  }

  handleUsernameFocus(event) {
    if(event.target.value == 'Username') {
      this.setState({username: ''});
    }
  }

  handlePasswordBlur(event) {
    if(event.target.value == '') {
      this.setState({password: 'Password'});
    }
  }

  handlePasswordFocus(event) {
    if(event.target.value == 'Password') {
      this.setState({password: ''});
    }
  }

  handleLoggedIn() {
    this.setState({isLoggedIn: true});
  }

  render() {
    const isLoggedIn = this.state.isLoggedIn;

    const logoStyle = {
    } 

    return (
      <div>
        {isLoggedIn ? (
            <div id="gameContainer" className="gameContainer">
              <UI />
              <Game />
            </div>
          ) : (
            <div className="container">
              <img src={logo} style={logoStyle}/>
              <div id="login">
                <form onSubmit={this.handleLoginClick} method="get">
                <fieldset className="clearfix">

                  <p><span className="fontawesome-user"></span>
                      <input type="text" 
                             value={this.state.username} 
                             onChange={this.handleUsernameChange} 
                             onBlur={this.handleUsernameBlur}
                             onFocus={this.handleUsernameFocus} />
                  </p>
                  <p><span className="fontawesome-lock"></span>
                      <input type="password" 
                             value={this.state.password} 
                             onChange={this.handlePasswordChange}
                             onBlur={this.handlePasswordBlur}
                             onFocus={this.handlePasswordFocus}/>
                  </p>
                  <p><input type="submit" value="Play"/></p>
                </fieldset>
                </form>

                <p>Not a member? <a href="#">Sign up now</a><span className="fontawesome-arrow-right"></span></p>

              </div>
            </div>

          )
        }    
      </div>
    );
  }
}
