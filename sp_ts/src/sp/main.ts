
import {GlobalVars} from './globalvars';
import {Network} from './network';

export function sendLogin() {
    var username = (<HTMLInputElement>document.getElementById("username")).value;
    var password = (<HTMLInputElement>document.getElementById("password")).value
  
    var login = '{"cmd": "login", "username": "' + username + 
                '", "password": "' + password + '"}';
  
    GlobalVars.socket.sendMessage(login)
  }

// when the page is loaded, create our game instance
window.addEventListener("load", () => {
    console.log('Opening websocket to server')
    
    GlobalVars.socket = new Network();
  });