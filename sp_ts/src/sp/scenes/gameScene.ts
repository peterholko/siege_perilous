/**
 * @author       Peter Holko
 * @copyright    2018 - 2019 Peter Holko
 */

export class GameScene extends Phaser.Scene {
  private phaserSprite: Phaser.GameObjects.Sprite;

  constructor() {
    super({
      key: "GameScene"
    });
  }

  preload(): void {
    console.log('Preload');
    //this.load.image("logo", "./static/src/boilerplate/assets/phaser.png");
  }

  create(): void {
    console.log('Create');
    //this.phaserSprite = this.add.sprite(400, 300, "logo");
  }
}
