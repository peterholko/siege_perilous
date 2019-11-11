export class GameContainer extends Phaser.GameObjects.Container {
  public id;
  public containerName;

  constructor(params) {

      super(params.scene, params.x, params.y, params.imageName);

      this.id = params.id;
      this.containerName = params.containerName;
  }

}