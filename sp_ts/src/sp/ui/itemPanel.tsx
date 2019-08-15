
import * as React from "react";
import HalfPanel from "./halfPanel";

interface ItemPanelProps {
  itemData,
}

export default class ItemPanel extends React.Component<ItemPanelProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };
    
  }

  render() {
    const itemName = this.props.itemData.name;
    const imageName = itemName.toLowerCase().replace(/ /g, '') + '.png'
    const effects = [];
    var produces = '';

    if(this.props.itemData.hasOwnProperty('effects')) {

      for(var i = 0; i < this.props.itemData.effects.length; i++) {
        var effect = this.props.itemData.effects[i];
        var type = ''
        var value = ''

        if(effect.type.indexOf('%') != -1) {
          type = effect.type.replace('%', '');

          if(effect.value > 0) {
            value = type + '+' + (effect.value * 100)+ '%';
          } else {
            value = type + (effect.value * 100)+ '%';
          }

        } else {
          value = type + effect.value;     
        }

        effects.push(<tr key={i}>
          <td>{value}</td>
        </tr>)
      }
    }

    if(this.props.itemData.hasOwnProperty('produces')) {
      produces = this.props.itemData.produces.join();
    }

    const itemStyle = {
      transform: 'translate(-185px, 25px)',
      position: 'fixed'
    } as React.CSSProperties

    const spanNameStyle = {
      transform: 'translate(-323px, 85px)',
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
      fontSize: '12px',
      width: '300px'
    } as React.CSSProperties

    const tableStyle2 = {
      transform: 'translate(-50px, 15px)',
      position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px'
    } as React.CSSProperties

    return (
      <HalfPanel left={false} 
                 panelType={'item'} 
                 hideExitButton={false}>
        <img src={'/static/art/' + imageName} style={itemStyle} />
        <span style={spanNameStyle}>
          {itemName} x {this.props.itemData.quantity}
        </span>
        <table style={tableStyle}>
          <tbody>
          <tr>
            <td>Class: </td>
            <td>{this.props.itemData.class}</td>
          </tr>
          <tr>
            <td>Subclass: </td>
            <td>{this.props.itemData.subclass}</td>
          </tr>
          <tr>
            <td>Weight: </td>
            <td>
              {this.props.itemData.weight} per unit 
              ({this.props.itemData.quantity * this.props.itemData.weight})
            </td>
          </tr>
          <tr>
            <td>Produces: </td>
            <td >{produces}</td>
          </tr>
          <tr>
            <td>Effects: </td>
            <td>
              <table style={tableStyle2}>
                <tbody>
                  {effects}
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

