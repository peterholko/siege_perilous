
import * as React from "react";
import HalfPanel from "./halfPanel";
import { Global } from "../global";
import ResourceItem from "./resourceItem";

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

    let numResources = this.props.tileData.resources.length + 
                       this.props.tileData.unrevealed;

    let discoveredResources = this.props.tileData.resources.length;

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
        <ResourceItem key={i}
                      resourceName={resource.name}
                      quantity={resource.quantity}
                      index={i}
                      showQuantity={false}/>
      )
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

    const resDivStyle = {
      transform: 'translate(15px, -90px)',
      position: 'fixed',
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
            <td>Discovered Resources: </td>
            <td>{discoveredResources} / {numResources}</td>
          </tr>
          </tbody>
        </table>
        <div style={resDivStyle}>
          {resources}
        </div>
      </HalfPanel>
    );
  }
}

/*
            <td>Resources Found: </td>
            <td>
              <table style={tableStyle2}>
                <tbody>
                  {resources}
                </tbody>
              </table>
            </td>
*/