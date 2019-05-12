export class Tile extends Phaser.GameObjects.Image {

    private tileImages;

    constructor(params) {
        super(params.scene, params.x, params.y, params.key);

        this.tileImages = params.tileImages;

        this.initImage();
    }

    private initImage(): void {
        this.setOrigin(0.0);
      }

}