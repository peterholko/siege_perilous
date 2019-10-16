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
import warrior from "art/warrior_single.png";
import ranger from "art/ranger_single.png";
import mage from "art/mage_single.png";
import halfpanel from "ui/halfpanel.png";

export default class LoginControl extends React.Component<any, any> {
  constructor(props) {
    super(props);

    this.state = {
      hideLoginForm : false,
      hideSelectClass : true,
      hideGame: true,
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

    this.handleWarriorSelect = this.handleWarriorSelect.bind(this);

    Global.gameEmitter.on(NetworkEvent.SELECT_CLASS, this.handleSelectClass, this);
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

  handleSelectClass() {
    this.setState({hideLoginForm: true,
                   hideSelectClass: false,
                   hideGame: true});
  }

  handleLoggedIn() {
    this.setState({hideLoginForm: true,
                   hideSelectClass: true,
                   hideGame: false});
  }

  handleWarriorSelect() {
    Network.sendSelectedClass("Warrior");
  }

  render() {
    const logoStyle = {
    } 

    const warriorStyle = {
      transform: 'translate(40px, 100px)',
      position: 'fixed'
    } as React.CSSProperties

    const rangerStyle = {
      transform: 'translate(140px, 100px)',
      position: 'fixed'
    } as React.CSSProperties

    const mageStyle = {
      transform: 'translate(240px, 100px)',
      position: 'fixed'
    } as React.CSSProperties

    const selectClassStyle = {
      top: '50%',
      left: '50%',
      width: '360px',
      height: '323px',
      marginTop: '-161px',
      marginLeft: '-180px',
      position: 'fixed'
    } as React.CSSProperties

    const selectClassBGStyle = {
      position: 'fixed',
      WebkitTransform: 'rotate(90deg)',
      transform: 'rotate(90deg) translate(-19px, -18px)'
    } as React.CSSProperties

    const selectHeroText = {
      transform: 'translate(0px, 50px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '16px',
      width: '360px'
    } as React.CSSProperties

    const warriorText = {
      transform: 'translate(48px, 175px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '14px',
    } as React.CSSProperties

    const rangerText = {
      transform: 'translate(152px, 175px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '14px',
    } as React.CSSProperties

    const mageText = {
      transform: 'translate(256px, 175px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '14px',
    } as React.CSSProperties

    return (
      <div>
        {!this.state.hideLoginForm && (
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

                {/*<p>Not a member? <a href="#">Sign up now</a><span className="fontawesome-arrow-right"></span></p>*/}

              </div>
            </div>

          )
        }   

        {!this.state.hideSelectClass && (
          <div style={selectClassStyle}>
              <img src={halfpanel} style={selectClassBGStyle} />
              <span style={selectHeroText}>Select Your Hero</span>
              <img src={warrior} style={warriorStyle} onClick={this.handleWarriorSelect}/>
              <span style={warriorText}>Warrior</span>
              <img src={ranger} style={rangerStyle}/>
              <span style={rangerText}>Ranger</span>
              <img src={mage} style={mageStyle}/>
              <span style={mageText}>Mage</span>
          </div>          
        )}

        {!this.state.hideGame && (
            <div id="gameContainer" className="gameContainer">
              <UI />
              <Game />
            </div>
          )
        }

 
      </div>
    );
  }
}
