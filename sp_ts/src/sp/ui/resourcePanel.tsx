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
      transform: 'translate(20px, -240px)',
      position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px'
    } as React.CSSProperties

    const tableStyle2 = {
      //transform: 'translate(-5px, -5px)',
      //position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px'
    } as React.CSSProperties

    var characteristics = [];

    if(this.props.resourceData.characteristics) {
      for(var i = 0; i < this.props.resourceData.characteristics.length; i++) {
        characteristics.push(<tr key={i}>
                      <td>+[{this.props.resourceData.characteristics[i].min} - {this.props.resourceData.characteristics[i].max}] {this.props.resourceData.characteristics[i].name}</td>
                    </tr>);
      }
    }


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
              <td>{this.props.resourceData.quantityLabel}</td>
            </tr>

            <tr>
              <td>Yield: </td>
              <td>{this.props.resourceData.yieldLabel}</td>
            </tr>
            <tr><td></td></tr>
            <tr>
              <td colSpan={2}>
                <table style={tableStyle2}>
                  <tbody>
                    {characteristics}
                  </tbody>
                </table>
              </td>
            </tr>        
          </tbody>
        </table>
      </HalfPanel>
    );
  }
}

