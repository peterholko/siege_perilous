
import * as React from "react";
import attackspanel from "ui_comp/attacksframe.png"

const MAX_ATTACKS = 6;

interface AttacksProp {
  attacks
}

export default class AttacksPanel extends React.Component<AttacksProp, any> {
  constructor(props) {
    super(props);

  }
  
  render() {

    var attacks = [];

    var startingIndex = 0;

    if(this.props.attacks.length > MAX_ATTACKS) {
        startingIndex = this.props.attacks.length - MAX_ATTACKS;        
    }

    var renderingIndex = 0;

    for(var i = startingIndex; i < this.props.attacks.length; i++) {

        var xPos = 3 + renderingIndex * 17;
        renderingIndex++;

        const style = {
            transform: 'translate(' + xPos + 'px, ' + 3 + 'px)',
            position: 'fixed'
          } as React.CSSProperties

        attacks.push(<img key={i} src={'/static/art/ui/small_' + this.props.attacks[i] + '.png'}
        style={style}/>)
    }

    const attacksStyle = {
      bottom: '85px',
      left: '50%',
      marginLeft: '-130px',
      position: 'fixed',
      zIndex: 6
    } as React.CSSProperties

    const panelStyle = {
      position: 'fixed'
    } as React.CSSProperties

    return (
      <div style={attacksStyle} >
          <img src={attackspanel} style={panelStyle}/> 
          {attacks}
      </div>
    );
  }
}