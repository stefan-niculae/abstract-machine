module.exports = {
    entry: './app/interface.coffee',
    output: {filename: 'app/bundle.js'},
    module: {
        rules: [{
            test: /\.coffee$/,
            use: ['coffee-loader'] // compile coffee-script
        }, {
            test: /\.ne$/,
            use: ['coffee-loader', 'nearley-loader'] // compile nearley grammar
        }, {
            test: /\.sass/,
            use: ['style-loader', 'css-loader', 'sass-loader']
        }]
    },
    resolve: {extensions: [".ne", ".coffee", ".js", '.sass']}
};
