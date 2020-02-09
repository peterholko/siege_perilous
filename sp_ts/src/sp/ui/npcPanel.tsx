import * as React from "react";
import HalfPanel from "./halfPanel";
import { Global } from "../global";

interface NPCPanelProps {
  npcData,
}

export default class NPCPanel extends React.Component<NPCPanelProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };
    
  }

  render() {
    let imagePath = '/static/art/' + this.props.npcData.image  + '_single.png';

    const npcStyle = {
      transform: 'translate(-185px, 25px)',
      position: 'fixed'
    } as React.CSSProperties

    const tableStyle = {
      transform: 'translate(20px, -250px)',
      position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px'
    } as React.CSSProperties

    return (
      <HalfPanel left={true} 
                 panelType={'npc'} 
                 hideExitButton={false}>

        <img src={imagePath} style={npcStyle} />
        <table style={tableStyle}>
          <tbody>
          <tr>
            <td>Name: </td>
            <td>{this.props.npcData.name}</td>
          </tr>
          <tr>
            <td>Effects: </td>
            <td>{this.props.npcData.effects}</td>
          </tr>
          </tbody>
        </table>
      </HalfPanel>
    );
  }
}

