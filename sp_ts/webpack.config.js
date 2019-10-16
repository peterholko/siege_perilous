var path = require('path');
var pathToPhaser = path.join(__dirname, '/node_modules/phaser/');
var phaser = path.join(pathToPhaser, 'dist/phaser.min.js');

module.exports = {
  mode: "production",
  //devtool: "source-map",

  entry: './src/sp/main.tsx',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'sp2.js',
    library: 'SP'
  },
  module: {
    rules: [
      { 
        test: /\.ts(x?)$/, 
        exclude: /node_modules/,
        loader: 'ts-loader', 
        exclude: '/node_modules/' 
      },
      { 
        test: /phaser\.js$/, 
        loader: 'expose-loader?Phaser' 
      },
      {
        test: /\.css$/i,
        loader: 'style-loader!css-loader',
      },
      {
        test: /\.(jpe?g|png|gif|woff|woff2|eot|ttf|svg)(\?[a-z0-9=.]+)?$/,
        loader: 'url-loader?limit=140000' 
      }
    ]
  },
  resolve: {
    extensions: ['.ts', '.tsx', '.js'],
    alias: {
      phaser: phaser,
      ui: './../../../priv/static/art/ui/',
      ui_comp: './../../../../priv/static/art/ui/',
      art: './../../../priv/static/art/'
    }
  },
};
