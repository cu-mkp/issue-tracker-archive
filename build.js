const path = require("path"); // Module for handling path names.
const fs = require("fs"); // Module for accessing file system.
const handlebars = require("handlebars"); // Module for templating.
const showdown = require("showdown"); // Module for rendering GitHub-flavored Markdown as HTML.

var converter;

function converterSetup() {
    let options = {
        simpleLineBreaks: true
        // Add other GitHub-flavored Markdown features here.
    };
    converter = new showdown.Converter(options);
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

function buildIssuePage(issuesJSON, commentsJSON, template) {

    // Assertion makes sure the following if statement makes sense by logically coupling these two values.
    console.assert((issuesJSON["comments"]==0 && !commentsJSON) || (issuesJSON["comments"]!=0 && commentsJSON));

    issuesJSON["body"] = convertMarkdown(issuesJSON["body"]);
    if (commentsJSON) {
        for (let i=0; i<commentsJSON.length; i++) {
            commentsJSON[i]["body"] = convertMarkdown(commentsJSON[i]["body"]);
        }
    }

    let context = {
        "issue" : issuesJSON,
        "comments" : commentsJSON
    };

    let output = template(context);
    return output;
}

function convertMarkdown(md) {
    let html = converter.makeHtml(md);
    return html;
}

function writeIssuePage(html, destination) {
    fs.writeFile(destination, html, (err) => {
        if (err) {
            console.log("Error writing file to disk:", err);
            return;
        }
        console.log("Wrote to ", destination);
    });
}

function loadJSON(filename) {
    let jsonString = null;
    let obj = null;
    try {
        jsonString = fs.readFileSync(filename, "utf8");
    }
    catch (err) {
        console.log("Error reading file from disk:", err);
        return;
    }

    try {
        obj = JSON.parse(jsonString);
        return obj;
    }
    catch (err) {
        console.log("Error parsing JSON string:", err);
        return;
    }
}

function main(issuesFile, commentsFile, templateFile, titlePartialFile, issuePartialFile, commentsPartialFile, destinationDirectory) {

    converterSetup();

    const template = loadTemplate(templateFile);
    registerPartial("title", titlePartialFile);
    registerPartial("issue", issuePartialFile);
    registerPartial("comments", commentsPartialFile);

    const issues = loadJSON(issuesFile);
    const comments = loadJSON(commentsFile);

    for (let i=0; i<issues.length; i++) {
        let issueJSON = issues[i];
        let commentsJSON = comments[issues[i]["id"]];
        let page = buildIssuePage(issueJSON, commentsJSON, template);
        let outfile = path.join(destinationDirectory, "issue" + issueJSON["number"].toString() + ".html");
        writeIssuePage(page, outfile);
    }
}

main(
    "./_data/issues.json",
    "./_data/comments.json",
    "./_templates/default.html",
    "./_templates/title.handlebars",
    "./_templates/issue.handlebars",
    "./_templates/comments.handlebars",
    "./_site/"
);
