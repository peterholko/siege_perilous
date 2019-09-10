
import * as React from "react";
import HalfPanel from "./halfPanel";
import { Global } from "../global";

interface TilePanelProps {
  tileData,
}

export default class TilePanel extends React.Component<TilePanelProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };
  }

  render() {
    const x = this.props.tileData.x;
    const y = this.props.tileData.y;
    const tileIndex = x + '_' + y; 
    const tileState = Global.tileStates[tileIndex];
    const tiles = [...tileState.tiles]; //Deep copy
    const resources = []

    //The default Grass was "above" forest, solved it via sort
    var tileId = tiles.sort().reverse()[0];
    var imageName = Global.tileset[tileId].image;

    var passable = (this.props.tileData.passable ? 'Yes' : 'No');
    var movementCost = String(this.props.tileData.mc * 100);
    movementCost = movementCost + '%';
    var sanctuary = (this.props.tileData.sanctuary ? 'Yes' : 'No');
    var tileStyle;

    for(var i = 0; i < this.props.tileData.resources.length; i++) {
      var resource = this.props.tileData.resources[i];

      resources.push(
      <tr key={i}>
        <td>{resource.name}</td>
        <td>{resource.quantity}</td>
      </tr>)
    }

    //Manual size adjustments
    if(tileId == 19) {
      tileStyle = {
        transform: 'translate(-205px, 10px)',
        width: '110px',
        position: 'fixed'
      } as React.CSSProperties
    } else if (tileId == 32) {
      tileStyle = {
        transform: 'translate(-225px, -20px)',
        width: '150px',
        position: 'fixed'
      } as React.CSSProperties
    }
    else {
      tileStyle = {
        transform: 'translate(-185px, 25px)',
        position: 'fixed'
      } as React.CSSProperties
    }

    const tableStyle = {
      transform: 'translate(20px, -220px)',
      position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px'
    } as React.CSSProperties

    const tableStyle2 = {
      transform: 'translate(-130px, 10px)',
      position: 'fixed',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px'
    } as React.CSSProperties

    const spanNameStyle = {
      transform: 'translate(-323px, 110px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '323px'
    } as React.CSSProperties

    return (
      <HalfPanel left={true} 
                 panelType={'tile'} 
                 hideExitButton={false}>
        <img src={'/static/art/' + imageName} style={tileStyle} />
        <span style={spanNameStyle}>{this.props.tileData.name} ({x}, {y})</span>
        <table style={tableStyle}>
          <tbody>
          <tr>
            <td>Passable: </td>
            <td>{passable}</td>
          </tr>
          <tr>
            <td>Movement Cost: </td>
            <td>{movementCost}</td>
          </tr>
          <tr>
            <td>Defense Bonus: </td>
            <td>{this.props.tileData.def}</td>
          </tr>
          <tr>
            <td>Sanctuary: </td>
            <td>{sanctuary}</td>
          </tr>
          <tr>
            <td>Wildness: </td>
            <td>{this.props.tileData.wildness}</td>
          </tr>
          <tr>
            <td>Unrevealed Resources: </td>
            <td>{this.props.tileData.unrevealed}</td>
          </tr>
          <tr>
            <td>Resources Found: </td>
            <td>
              <table style={tableStyle2}>
                <tbody>
                  {resources}
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

