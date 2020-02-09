import * as React from "react";
import HalfPanel from "./halfPanel";
import { Global } from "../global";
import advancebutton from "ui_comp/upgradebutton.png";
import rightbutton from "ui_comp/rightbutton.png";
import { Network } from "../network";

interface HAPProps {
  advanceData,
}

export default class HeroAdvancePanel extends React.Component<HAPProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };
   
    this.handleAdvanceClick = this.handleAdvanceClick.bind(this);
  }

  handleAdvanceClick() {
    Network.sendAdvance(Global.heroId);
  }

  render() {
    let heroRank = this.props.advanceData.rank;

    //TODO Don't render image for Max Rank

    let heroImage = heroRank.toLowerCase().replace(/\s/g, '');
    let heroImagePath = '/static/art/' + heroImage  + '_single.png';

    let nextRankName = this.props.advanceData.next_rank;
    let nextRankImage = nextRankName.toLowerCase().replace(/\s/g, '');
    let nextRankImagePath = '/static/art/' + nextRankImage + '_single.png';

    let nextRankXp = 'XP: ' + this.props.advanceData.total_xp + ' / ' + this.props.advanceData.req_xp;

    const heroStyle = {
      transform: 'translate(-290px, 100px)',
      position: 'fixed'
    } as React.CSSProperties

    const nextRankImageStyle = {
      transform: 'translate(-110px, 100px)',
      position: 'fixed'
    } as React.CSSProperties

    const heroNameStyle = {
      transform: 'translate(-300px, 175px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '100px'
    } as React.CSSProperties

    const nextRankNameStyle = {
      transform: 'translate(-120px, 175px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '100px'
    } as React.CSSProperties

    const reqXpStyle = {
      transform: 'translate(-323px, 225px)',
      position: 'fixed',
      textAlign: 'center',
      color: 'white',
      fontFamily: 'Verdana',
      fontSize: '12px',
      width: '323px'
    } as React.CSSProperties

    const rightArrowStyle = {
      transform: 'translate(-185px, 125px)',
      position: 'fixed'
    } as React.CSSProperties

    const advanceStyle = {
      transform: 'translate(-185px, 285px)',
      position: 'fixed'
    } as React.CSSProperties

    return (
      <HalfPanel left={false} 
                 panelType={'advance'} 
                 hideExitButton={false}>
        <img src={heroImagePath} style={heroStyle} />
        <img src={nextRankImagePath} style={nextRankImageStyle} />
        <img src={rightbutton} style={rightArrowStyle} />
        <span style={reqXpStyle}>{nextRankXp}</span>
        <span style={heroNameStyle}>{heroRank}</span>
        <span style={nextRankNameStyle}>{nextRankName}</span>

        <img src={advancebutton} style={advanceStyle} onClick={this.handleAdvanceClick} /> 
      </HalfPanel>
    );
  }
}

