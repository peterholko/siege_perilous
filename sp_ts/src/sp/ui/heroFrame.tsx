import * as React from "react";
import { Global } from "../global";
import heroring from "ui_comp/heroring.png";
import hpframe from "ui_comp/hpframe.png";
import statbg from "ui_comp/statbg.png";
import hpbar from "ui_comp/hpbar.png";
import stabar from "ui_comp/stabar.png";
import manabar from "ui_comp/manabar.png";
import { NetworkEvent } from "../networkEvent";
import { STAT_BAR_WIDTH, STAT_BAR_HEIGHT } from "../config";

interface HeroFrameProps {
}

export default class HeroFrame extends React.Component<HeroFrameProps, any> {
  constructor(props) {
    super(props);

    this.state = {
      hpBarWidth : STAT_BAR_WIDTH,
      staBarWidth : STAT_BAR_WIDTH,
      manaBarWidth: STAT_BAR_WIDTH,
      hideHero : true 
    };
  }

  componentDidMount() {
    Global.gameEmitter.on(NetworkEvent.PERCEPTION, this.handlePerception, this);
    Global.gameEmitter.on(NetworkEvent.STATS, this.handleStats, this);
  }

  handlePerception() {
    this.setState({hideHero: false});
  }

  handleStats() {
    console.log('UI handleStats');
    const hpRatio = Global.heroHp / Global.heroMaxHp;
    const hpBarWidth = hpRatio * STAT_BAR_WIDTH;

    this.setState({hpBarWidth: hpBarWidth});
  }

  render() {
    let imagePath = '';

    if(Global.heroId in Global.objectStates) {
      let imageName = Global.objectStates[Global.heroId].image.toLowerCase().replace(/\s/g, '');
      imagePath = '/static/art/' + imageName  + '_single.png';
    }

    const heroringStyle = {
      transform: 'translate(8px, 21px)',
      zIndex: 3,
      position: 'fixed'
    } as React.CSSProperties

    const hpframeStyle = {
      transform: 'translate(41px, 10px)',
      zIndex: 2,
      position: 'fixed'
    } as React.CSSProperties

    const hpbgStyle = {
      transform: 'translate(95px, 17px)',
      zIndex: 3,
      position: 'fixed'
    } as React.CSSProperties

    const stabgStyle = {
      transform: 'translate(95px, 35px)',
      zIndex: 3,
      position: 'fixed'
    } as React.CSSProperties

    const manabgStyle = {
      transform: 'translate(95px, 53px)',
      zIndex: 3,
      position: 'fixed'
    } as React.CSSProperties

    const hpBarStyle  = {
      transform: 'translate(97px, 19px)',
      width: this.state.hpBarWidth + 'px',
      height: STAT_BAR_HEIGHT + 'px',
      zIndex: 4,
      position: 'fixed' 
    } as React.CSSProperties
 
    const staBarStyle  = {
      transform: 'translate(97px, 37px)',
      width: this.state.staBarWidth + 'px',
      height: STAT_BAR_HEIGHT + 'px',
      zIndex: 4,
      position: 'fixed' 
    } as React.CSSProperties
  
    const manaBarStyle  = {
      transform: 'translate(97px, 55px)',
      width: this.state.manaBarWidth + 'px',
      height: STAT_BAR_HEIGHT + 'px',
      zIndex: 4,
      position: 'fixed' 
    } as React.CSSProperties
 
    const heroStyle = {
      transform: 'translate(13px, 24px)',
      zIndex: 3,
      position: 'fixed'
    } as React.CSSProperties

    return (
      <div>
          <img src={heroring} style={heroringStyle}/>
          <img src={hpframe} style={hpframeStyle}/>

          <img src={statbg} style={hpbgStyle}/>
          <img src={hpbar} style={hpBarStyle}/>
          <img src={statbg} style={stabgStyle}/>
          <img src={stabar} style={staBarStyle}/>
          <img src={statbg} style={manabgStyle}/>
          <img src={manabar} style={manaBarStyle}/>
          {!this.state.hideHero && 
            <img src={imagePath} style={heroStyle}/>
          }
      </div>
    );
  }
}

