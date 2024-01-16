import * as React from "react";
import { Global } from "../global";
import SelectBox from "./selectBox";
import { TILE, OBJ, LARGE_SCREEN_WIDTH, DEAD } from "../config";
import { Util } from "../util";
import { Tile } from "../objects/tile";
import { Obj } from "../obj";
import leftbutton from "ui_comp/leftbutton.png";
import rightbutton from "ui_comp/rightbutton.png";
import { GameEvent } from "../gameEvent";
import { NetworkEvent } from "../networkEvent";

const MAX_SELECT_BOXES = (window.innerWidth < LARGE_SCREEN_WIDTH ? 3 : 6);

interface SelectPanelProps {
  selectedTile: Tile,
  objIdsOnTile: any,
  selectedKey: any
}

export default class SelectPanel extends React.Component<SelectPanelProps, any> {
  constructor(props) {
    super(props);

    this.state = {
      startIndex: 0,
      showBorderSelectedKey: -1,
      objKilled: false
    };

    this.leftClick = this.leftClick.bind(this);
    this.rightClick = this.rightClick.bind(this);

    //Global.gameEmitter.on(GameEvent.SELECTBOX_CLICK, this.handleSelectBoxClick, this);
    Global.gameEmitter.on(NetworkEvent.DMG, this.handleDamage, this); 
  }

  leftClick() {
    this.setState({ startIndex: this.state.startIndex + MAX_SELECT_BOXES });
    Global.gameEmitter.emit(GameEvent.SELECT_PANEL_CLICK, {});
  }

  rightClick() {
    this.setState({ startIndex: this.state.startIndex - MAX_SELECT_BOXES });
    Global.gameEmitter.emit(GameEvent.SELECT_PANEL_CLICK, {});
  }

  handleSelectBoxClick(eventData) {
    this.setState({showBorderSelectedKey: eventData.selectedKey});
  }

  handleDamage(eventData) {
    for(var i = 0; i < this.props.objIdsOnTile.length; i++) {
      if(eventData.state == DEAD && eventData.targetid == this.props.objIdsOnTile[i]) {
        this.setState({objKilled: true});
      }
    }
  }

  render() {
    console.log("Select Panel props: " + JSON.stringify(this.props));

    if (this.props.selectedTile == null) {
      return null;
    }

    const boxes = [];
    const tile = this.props.selectedTile as Tile;
    var objIdsOnTile = [...this.props.objIdsOnTile]; // deep copy

    var hideLeftButton = true;
    var hideRightButton = true;

    //Add terain tile first
    var tileIndex = tile.hexX + '_' + tile.hexY;
    var tileState = Global.tileStates[tileIndex];
    const tiles = [...tileState.tiles]; //Deep copy

    //The default Grass was "above" forest, solved it via sort
    var tileId = tiles.sort().reverse()[0];
    var imageName = Global.tileset[tileId].image;
    var imageStyle;
    var selectBoxPos = 0;

    //Manual adjustments
    if (tileId == 19) { //Forest
      imageStyle = {
        top: '9px',
        right: '26px',
        width: '75px',
        height: '75px',
        position: 'fixed'
      } as React.CSSProperties;
    } else if (tileId == 32) { //Mountain
      imageStyle = {
        top: '-5px',
        right: '22px',
        width: '90px',
        position: 'fixed'
      } as React.CSSProperties
    } else {
      imageStyle = {
        top: '24px',
        right: '49px',
        width: '45px',
        height: '45px',
        position: 'fixed'
      } as React.CSSProperties
    }

    var style = {
      top: '10px',
      right: '35px',
      position: 'fixed'
    } as React.CSSProperties

    var maxIndex = (Math.floor(this.state.startIndex / MAX_SELECT_BOXES) + 1) * MAX_SELECT_BOXES;

    console.log("objIdsOnTile: " + objIdsOnTile);
    //Add a fake object to represent the tile
    objIdsOnTile.unshift(-1);
    console.log("objIdsOnTile: " + objIdsOnTile);

    //Draw tile only if startIndex == 0
    if (this.state.startIndex == 0) {

      /*var showBorder = (this.state.showBorderSelectedKey.x == tile.hexX) && 
                       (this.state.showBorderSelectedKey.y == tile.hexY);*/
      var showBorder = this.props.selectedKey.type == TILE;

      boxes.push(<SelectBox key={-1}
        pos={0}
        selectedKey={{ type: TILE, x: tile.hexX, y: tile.hexY }}
        imageName={imageName}
        style={style}
        imageStyle={imageStyle} 
        showBorder={showBorder}
        showGravestone={false}
        />)

      //Increment selectBoxPos due to tile
      selectBoxPos++;
    }


    if (objIdsOnTile.length < maxIndex) {
      maxIndex = objIdsOnTile.length;
    }

    if (objIdsOnTile.length > maxIndex) {
      hideLeftButton = false;
    }

    if (this.state.startIndex > 0) {
      hideRightButton = false;
    }


    for (var i = this.state.startIndex; i < maxIndex; i++) {
      const objId: integer = Number(objIdsOnTile[i]);

      if (objId == -1) {
        //Skip the fake object that represents the tile
        continue
      }

      console.log("objId: " + objId);
      console.log(Global.objectStates[objId]);

      if (objId in Global.objectStates) {

        if (Util.isSprite(Global.objectStates[objId].image)) {
          var imageName = Global.objectStates[objId].image + '_single.png';
        } else {
          var imageName = Global.objectStates[objId].image + '.png';
        }

        var rightPos = i % MAX_SELECT_BOXES;

        var style = {
          top: '10px',
          right: 35 + (75 * rightPos) + 'px',
          position: 'fixed'
        } as React.CSSProperties

        //TODO this should only happen on tile click not on selectbox click
        //By default select the further to the left object on tile click
        /*if(i == (maxIndex - 1)) {
          Global.selectedKey = {type: OBJ, id: objId};
        }*/

        //var showBorder = this.state.showBorderSelectedKey.id == objId;
        var showBorder = this.props.selectedKey.id == objId;
        var isDead = Global.objectStates[objId].state == DEAD;
        console.log("isDead: " + isDead);

        boxes.push(<SelectBox key={i}
          pos={selectBoxPos}
          selectedKey={{ type: OBJ, id: objId }}
          imageName={imageName}
          style={style}
          showBorder={showBorder}
          showGravestone={isDead}
          />);

        selectBoxPos++;
      }
    }



    const rightStyle = {
      top: '27px',
      right: '0px',
      position: 'fixed',
      width: '35px',
      height: '35px'
      //zIndex: 5
    } as React.CSSProperties

    const leftStyle = {
      top: '27px',
      right: '259px',
      position: 'fixed',
      width: '35px',
      height: '35px'
      //zIndex: 5
    } as React.CSSProperties

    return (
      <div>
        {!hideLeftButton &&
          <img src={leftbutton} style={leftStyle} onClick={this.leftClick} />}
        {boxes}
        {!hideRightButton &&
          <img src={rightbutton} style={rightStyle} onClick={this.rightClick} />}
      </div>
    );
  }
}

