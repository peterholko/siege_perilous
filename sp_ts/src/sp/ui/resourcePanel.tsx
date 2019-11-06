import * as React from "react";
import HalfPanel from "./halfPanel";
import { Global } from "../global";

interface ResourceProps {
  resourceData,
}

export default class ResourcePanel extends React.Component<ResourceProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };
   
  }

  render() {
    const imageName = this.props.resourceData.name.replace(/\s/g, '').toLowerCase();

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

    return (
      <HalfPanel left={false} 
                 panelType={'resource'} 
                 hideExitButton={false}>
        <img src={'/static/art/items/' + imageName + '.png'} style={imageStyle} />
        <span style={spanNameStyle}>{this.props.resourceData.name}</span>
        <table style={tableStyle}>
          <tbody>
            <tr>
              <td>Quantity: </td>
              <td>{this.props.resourceData.quantity}</td>
            </tr>
          </tbody>
        </table>
      </HalfPanel>
    );
  }
}

