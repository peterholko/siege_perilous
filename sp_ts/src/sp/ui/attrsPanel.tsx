import * as React from "react";
import HalfPanel from "./halfPanel";
import { Global } from "../global";

interface AttrsPanelProps {
  attrsData,
}

export default class AttrPanel extends React.Component<AttrsPanelProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };
   
  }

  render() {
    var objId = this.props.attrsData.id;
    var imageName = Global.objectStates[objId].image;
    imageName = imageName.replace(/ /g, '') + '_single.png';
    var name = Global.objectStates[objId].name;

    const attrs = [];

    const imageStyle = {
      transform: 'translate(-195px, 25px)',
      position: 'fixed'
    } as React.CSSProperties

    const spanNameStyle = {
      transform: 'translate(-323px, 90px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '323px'
    } as React.CSSProperties

    const tableStyle = {
      transform: 'translate(20px, -250px)',
      position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px'
    } as React.CSSProperties

    var key = 0;

    for(var attr in this.props.attrsData.attrs) {
      attrs.push(<tr key={key}>
                    <td>{attr}</td>
                    <td>{this.props.attrsData.attrs[attr]}</td>
                  </tr>);

      key++;
    }

    return (
      <HalfPanel left={false} 
                 panelType={'attrs'} 
                 hideExitButton={false}>
        <img src={'/static/art/' + imageName} style={imageStyle} />
        <span style={spanNameStyle}>{name}</span>
        <table style={tableStyle}>
          <tbody>
            {attrs}      
          </tbody>
        </table>
      </HalfPanel>
    );
  }
}

