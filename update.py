import json
import requests
from config import token # Get the secret token from a file called config.py.

PER_PAGE = 100
PAGES = 30
REQUEST_URL = "https://api.github.com/repos/cu-mkp/m-k-manuscript-data/issues"

def get_issues(request_url=REQUEST_URL):
    issues = []
    for page in range(1, PAGES+1): # Is there a better way to make sure we get all the issues than by picking a large number of pages?
        payload = {"state":"all", "page":page, "per_page":PER_PAGE, "sort":"created", "direction":"asc"}
        r = requests.get(request_url, params=payload, headers={"Authorization":f"token {token}"})
        assert r.status_code == 200
        print(f"Found {len(r.json())} issues on page {page}.")
        issues.extend(r.json())
        if len(r.json()) < PER_PAGE:
            break
    print(f"Found {len(issues)} issues.")
    return issues

def get_comments(issues):
    comments = {}
    for issue in issues:
        if issue["comments"] == 0:
            continue
        comments[issue["id"]] = []
        for page in range(1, issue["comments"]//PER_PAGE+2):
            payload = {"state":"all", "page":page, "per_page":PER_PAGE, "sort":"created", "direction":"asc"}
            r = requests.get(issue["comments_url"], params=payload, headers={"Authorization":f"token {token}"})
            assert r.status_code == 200
            comments[issue["id"]].extend(r.json())
        assert len(comments[issue["id"]]) == issue["comments"]
        print(f"Found {len(comments[issue['id']])}/{issue['comments']} comments for issue #{issue['number']}.")
    return comments

def write_issues(issues):
    with open("issues.json", "w") as fp:
        json.dump(issues, fp, indent=4)

def write_comments(comments):
    with open("comments.json", "w") as fp:
        json.dump(comments, fp, indent=4)

issues = get_issues()
comments = get_comments(issues)
write_issues(issues)
write_comments(comments)
