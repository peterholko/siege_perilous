
import * as React from "react";
import HalfPanel from "./halfPanel";
import { Global } from "../global";

interface SkillsPanelProps {
  skillsData,
}

export default class SkillsPanel extends React.Component<SkillsPanelProps, any> {
  constructor(props) {
    super(props);

    this.state = {
    };
   
  }

  render() {
    var objId = this.props.skillsData.id;
    var imageName = Global.objectStates[objId].image;
    imageName = imageName.replace(/ /g, '') + '_single.png';
    var name = Global.objectStates[objId].name;

    const skills = [];

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

    var key = 0;

    for(var skill in this.props.skillsData.skills) {
      skills.push(<tr key={key}>
                    <td>{skill}</td>
                    <td>{this.props.skillsData.skills[skill].level}</td>
                    <td>{this.props.skillsData.skills[skill].xp}</td>
                    <td>{this.props.skillsData.skills[skill].next}</td>
                  </tr>);

      key++;
    }

    return (
      <HalfPanel left={false} 
                 panelType={'skills'} 
                 hideExitButton={false}>
        <img src={'/static/art/' + imageName} style={imageStyle} />
        <span style={spanNameStyle}>{name}</span>
        <table style={tableStyle}>
          <tbody>
            <tr>
              <th>Name</th>
              <th>Level</th>
              <th>Xp</th>
              <th>Next Level</th>
            </tr>
            {skills}      
          </tbody>
        </table>
      </HalfPanel>
    );
  }
}

