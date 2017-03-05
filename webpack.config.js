module.exports = {
    entry: './app/main.coffee',
    output: {filename: 'app/bundle.js'},
    module: {
        rules: [{
            test: /\.coffee$/,
            use: ['coffee-loader'] // compile coffee-script
        }, {
            test: /\.ne$/,
            use: ['coffee-loader', 'nearley-loader']
        }]
    },
    resolve: {extensions: [".ne", ".coffee", ".js"]}
};
