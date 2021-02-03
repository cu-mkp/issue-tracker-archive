const path = require("path");
const fs = require("fs");
const Generator = require("./generator");

const ASSETS_DIRECTORY = "./assets/";
const ISSUES_FILE = "./_data/issues.json";
const COMMENTS_FILE = "./_data/comments.json";
const ISSUE_TEMPLATE_FILE = "./_templates/issue_layout.html";
const ISSUE_TITLE_PARTIAL_FILE = "./_templates/issue_title.handlebars";
const ISSUE_BODY_PARTIAL_FILE = "./_templates/issue_body.handlebars";
const ISSUE_COMMENTS_PARTIAL_FILE = "./_templates/issue_comments.handlebars";
const ISSUES_INDEX_TEMPLATE_FILE = "./_templates/issues_index.html";
const DESTINATION_DIRECTORY = "./_site/";

const SHOWDOWN_OPTIONS = {
    simpleLineBreaks: true
    // Add other GitHub-flavored Markdown features here.
};

const PARTIALS = {
    issue_title: ISSUE_TITLE_PARTIAL_FILE,
    issue_body: ISSUE_BODY_PARTIAL_FILE,
    issue_comments: ISSUE_COMMENTS_PARTIAL_FILE
};

var generator;

function main() {
    
    if (!fs.existsSync(DESTINATION_DIRECTORY)) {
        fs.mkdirSync(DESTINATION_DIRECTORY);
    }

    if (!fs.existsSync(path.join(DESTINATION_DIRECTORY, "issues"))) {
        fs.mkdirSync(path.join(DESTINATION_DIRECTORY, "issues"));
    }

    // Copy assets directory.
    if (fs.existsSync(ASSETS_DIRECTORY)) {
        copyFolderRecursiveSync(ASSETS_DIRECTORY, DESTINATION_DIRECTORY);
    }

    // Copy index.html file.
    fs.copyFileSync("./index.html", path.join(DESTINATION_DIRECTORY, "index.html"));

    generator = new Generator(ISSUE_TEMPLATE_FILE, PARTIALS, SHOWDOWN_OPTIONS);

    const issues = loadJSON(ISSUES_FILE);
    const comments = loadJSON(COMMENTS_FILE);

    // Write issue pages.
    for (let issue of issues) {
        let outfile = path.join(DESTINATION_DIRECTORY, "issues", issue.number.toString() + ".html");

        let context = prepareContext(issue, comments[issue.id]);

        let page = generator.generatePage(context);
        generator.write(page, outfile);
    }

    // Write issues/index.html page.
    const issueList = issues.map((issue) => { return {number: issue.number, name: issue.title, url: "/issues/" + issue.number.toString() + ".html"}; });
    generator.write(generator.generatePage({"issues": issueList}, generator.loadTemplate(ISSUES_INDEX_TEMPLATE_FILE)),
                    path.join(DESTINATION_DIRECTORY, "issues", "index.html"));
}

function copyFolderRecursiveSync( source, target ) {
    var files = [];

    // Check if folder needs to be created or integrated
    var targetFolder = path.join( target, path.basename( source ) );
    if ( !fs.existsSync( targetFolder ) ) {
        fs.mkdirSync( targetFolder );
    }

    // Copy
    if ( fs.lstatSync( source ).isDirectory() ) {
        files = fs.readdirSync( source );
        files.forEach( function ( file ) {
            var curSource = path.join( source, file );
            if ( fs.lstatSync( curSource ).isDirectory() ) {
                copyFolderRecursiveSync( curSource, targetFolder );
            } else {
                fs.copyFileSync( curSource, path.join(targetFolder, path.basename(curSource)) );
            }
        } );
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
