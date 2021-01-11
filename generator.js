const path = require("path"); // Module for handling path names.
const fs = require("fs"); // Module for accessing file system.
const handlebars = require("handlebars"); // Module for templating.
const showdown = require("showdown"); // Module for rendering GitHub-flavored Markdown as HTML.

function converterSetup(options) {
    let converter = new showdown.Converter(options);
    return converter
}

function loadTemplate(filename) {
    let source = null;
    try {
        source = fs.readFileSync(filename, "utf8"); 
    }
    catch (err) {
        console.log("Error reading file from disk:", err);
        return;
    }
    let template = handlebars.compile(source);
    return template;
}

function registerPartial(name, filename) {
    let source = null;
    try {
        source = fs.readFileSync(filename, "utf8");
    }
    catch (err) {
        console.log("Error reading file from disk:", err);
        return;
    }
    handlebars.registerPartial(name, source);
}

class Generator {

    constructor(handlebarsTemplateFile, handlebarsPartials, showdownConverterOptions) {

        this.template = loadTemplate(handlebarsTemplateFile);
        this.converter = converterSetup(showdownConverterOptions);

        for (let name in handlebarsPartials) {
            registerPartial(name.toString(), handlebarsPartials[name]);
        }

    }

    renderMarkdown(md) {
        let html = this.converter.makeHtml(md);
        return html;
    }

    generatePage(context, template) {
        /*
         * context : a JavaScript object to be rendered according to the Handlebars template.
         * template : a Handlebars compiled template. Defaults to the Generator object's instance template if undefined.
         */

        if (template === undefined) {
            template = this.template;
        }

        let output = template(context);
        return output;
    }

    write(content, destination) {
        fs.writeFile(destination, content, (err) => {
            if (err) {
                console.log("Error writing file to disk:", err);
                return;
            }
            console.log("Wrote to", destination);
        });
    }

}

module.exports = Generator;
