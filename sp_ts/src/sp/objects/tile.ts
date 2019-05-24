export class Tile extends Phaser.GameObjects.Image {

    private tileImages;
    public hexX;
    public hexY;

    constructor(params) {
        super(params.scene, params.x, params.y, params.key);

        this.tileImages = params.tileImages;
        this.hexX = params.hexX;
        this.hexY = params.hexY;

        this.initImage();
    }

    private initImage(): void {
        this.setOrigin(0.0);
      }

}