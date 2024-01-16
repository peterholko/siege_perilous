import * as React from "react";
import selectbox from "ui_comp/selectbox.png";
import selectboxborder from "ui_comp/selectboxborder.png";
import { Global } from "../global";
import { GameEvent } from "../gameEvent";
import { DEAD, TILE } from "../config";
import { NetworkEvent } from "../networkEvent";

interface SelectedKey {
  type: string,
  id?: integer,
  x?: integer,
  y?: integer,
}

interface SelectBoxProps {
  pos: integer,
  selectedKey: SelectedKey,
  imageName: string,
  style: React.CSSProperties,
  imageStyle?: React.CSSProperties,
  showBorder: boolean,
  showGravestone: boolean
}

export default class SelectBox extends React.Component<SelectBoxProps, any> {
  constructor(props) {
    super(props);

    this.handleClick = this.handleClick.bind(this);
  }

  handleClick() {
    console.log(this.props.selectedKey);

    var eventData = {
      'pos': this.props.pos,
      'selectedKey': this.props.selectedKey
    };

    Global.gameEmitter.emit(GameEvent.SELECTBOX_CLICK, eventData);
  }

  render() {

    return (
      <div onClick={this.handleClick}>
        <img src={selectbox} style={this.props.style} />
        {!this.props.showGravestone &&
          <img src={'/static/art/' + this.props.imageName} style={this.props.imageStyle || this.props.style} /> }
        {this.props.showGravestone &&
          <img src={'/static/art/gravestone.png'} style={this.props.style} />}
        {this.props.showBorder &&
          <img src={selectboxborder} style={this.props.style} />}
      </div>
    );
  }
}

