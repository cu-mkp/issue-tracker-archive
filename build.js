const path = require("path"); // Module for handling path names.
const fs = require("fs"); // Module for accessing file system.
const Handlebars = require("handlebars"); // Module for templating.

async function loadTemplate(filename) {
    let source = null;
    try {
        source = fs.readFileSync(filename, "utf8"); 
    }
    catch (err) {
        console.log("Error reading file from disk:", err);
        return;
    }
    let template = Handlebars.compile(source);
    return template;
}

async function registerPartial(name, filename) {
    let source = null;
    try {
        source = fs.readFileSync(filename, "utf8");
    }
    catch (err) {
        console.log("Error reading file from disk:", err);
        return;
    }
    Handlebars.registerPartial(name, source);
}

function buildIssuePage(issuesJSON, commentsJSON, template) {
    let context = {
        "issue" : issuesJSON,
        "comments" : commentsJSON
    };
    let output = template(context);
    return output
}

async function writeIssuePage(html, destination) {
    fs.writeFile(destination, html, (err) => {
        if (err) {
            console.log("Error writing file to disk:", err);
            return;
        }
        console.log("Wrote to ", destination);
    });
}

async function loadJSON(filename) {
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

async function main(issuesFile, commentsFile, templateFile, titlePartialFile, issuePartialFile, commentsPartialFile, destinationDirectory) {
    const template = await loadTemplate(templateFile);
    registerPartial("title", titlePartialFile);
    registerPartial("issue", issuePartialFile);
    registerPartial("comments", commentsPartialFile);

    const issues = await loadJSON(issuesFile);
    const comments = await loadJSON(commentsFile);

    for (let i=0; i<issues.length; i++) {
        let issueJSON = issues[i];
        let commentsJSON = comments[issues[i]["id"]];
        let page = buildIssuePage(issueJSON, commentsJSON, template);
        let outfile = path.join(destinationDirectory, "issue" + issueJSON["number"].toString() + ".html");
        writeIssuePage(page, outfile);
    }
}

main("./_data/issues.json", "./_data/comments.json", "./_templates/default.html", "./_templates/title.handlebars", "./_templates/issue.handlebars", "./_templates/comments.handlebars", "./_site/");
