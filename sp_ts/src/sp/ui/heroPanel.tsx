import * as React from "react";
import HalfPanel from "./halfPanel";
import { Global } from "../global";

interface HeroPanelProps {
  heroData,
}

export default class HeroPanel extends React.Component<HeroPanelProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };
    
  }

  render() {
    let imageName = Global.objectStates[Global.heroId].template.toLowerCase();
    let imagePath = '/static/art/' + imageName  + '_single.png';

    const heroStyle = {
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
                 panelType={'hero'} 
                 hideExitButton={false}>
        <img src={imagePath} style={heroStyle} />
        <table style={tableStyle}>
          <tbody>
          <tr>
            <td>Name: </td>
            <td>{this.props.heroData.name}</td>
          </tr>
          <tr>
            <td>Class: </td>
            <td>{this.props.heroData.class}</td>
          </tr>
          <tr>
            <td>Subclass: </td>
            <td>{this.props.heroData.subclass}</td>
          </tr>
          </tbody>
        </table>
      </HalfPanel>
    );
  }
}

