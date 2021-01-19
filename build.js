const path = require("path");
const fs = require("fs");
const Generator = require("./generator");

const ISSUES_FILE = "./_data/issues.json";
const COMMENTS_FILE = "./_data/comments.json";
const TEMPLATE_FILE = "./_templates/default.html";
const TITLE_PARTIAL_FILE = "./_templates/title.handlebars";
const ISSUE_PARTIAL_FILE = "./_templates/issue.handlebars";
const COMMENTS_PARTIAL_FILE = "./_templates/comments.handlebars";
const DESTINATION_DIRECTORY = "./_site/";

const SHOWDOWN_OPTIONS = {
    simpleLineBreaks: true
    // Add other GitHub-flavored Markdown features here.
};

const PARTIALS = {
    title: TITLE_PARTIAL_FILE,
    issue: ISSUE_PARTIAL_FILE,
    comments: COMMENTS_PARTIAL_FILE
};

var generator;

function main() {
    
    if (!fs.existsSync(DESTINATION_DIRECTORY)) {
        fs.mkdirSync(DESTINATION_DIRECTORY);
    }

    generator = new Generator(TEMPLATE_FILE, PARTIALS, SHOWDOWN_OPTIONS);

    const issues = loadJSON(ISSUES_FILE);
    const comments = loadJSON(COMMENTS_FILE);

    for (let issue of issues) {
        let outfile = path.join(DESTINATION_DIRECTORY, "issue" + issue.number.toString() + ".html");

        let context = prepareContext(issue, comments[issue.id]);

        let page = generator.generatePage(context);
        generator.write(page, outfile);
    }
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

function prepareContext(issue, comments) {

    // Assertion makes sure the following if statement makes sense by logically coupling these two values.
    console.assert((issue.comments==0 && !comments) || (issue.comments!=0 && comments));

    issue.body = generator.renderMarkdown(issue.body);
    if (comments) {
        for (let i=0; i<comments.length; i++) {
            comments[i].body = generator.renderMarkdown(comments[i].body);
        }
    }

    let context = {
        issue: issue,
        comments: comments
    };
    return context;
}

main();
