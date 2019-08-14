export class GameSprite extends Phaser.GameObjects.Sprite {
    public id;
    public imageName;

    constructor(params) {

        super(params.scene, params.x, params.y, params.imageName);

        this.id = params.id;
        this.imageName = params.imageName;

        this.initImage();
    }

    private initImage(): void {
        this.setOrigin(0);
      }

}