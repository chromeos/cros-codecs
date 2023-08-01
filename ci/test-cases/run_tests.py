#!/usr/bin/python3

import argparse
import os
import stat
import subprocess
import yaml
import requests
import time

URL_RUNS="https://api.github.com/repos/{repo}/actions/runs?head-sha={head_sha}&head-branch={head_branch}&per_page=2"
URL_RUN="https://api.github.com/repos/{repo}/actions/runs/{run_id}"

def run_fluster(codec, test_suite, skips, single_thread):
    print(f"  {codec} -> {test_suite} (skip: {skips})")
    cmd = ['python3', '/usr/bin/fluster_parser.py', '-ts', test_suite, '-d', f"ccdec-{codec}", '-t' '300']

    if single_thread:
        cmd.extend(['-j', '1'])
    if skips:
        for index, skip in enumerate(skips):
            cmd.extend(['-sv', skip] if not index else [skip])

    print(cmd)
    subprocess.run(cmd, check=False)

def retrieve_ccdec_github(sha, branch, repo, token):
    if os.path.exists("/opt/cros-codecs/ccdec"):
        os.environ['PATH'] = os.environ['PATH'] + ":/opt/cros-codecs"
        return True

    runs = requests.get(URL_RUNS.format(head_sha=sha, head_branch=branch, repo=repo), headers={"Accept": "application/vnd.github+json", "X-GitHub-Api-Version": "2022-11-28", "Authorization": f"Bearer {token}"}).json()

    found = False

    for run in runs['workflow_runs']:
        if run['name'] != 'Health check':
            continue

        artifacts = requests.get(run['artifacts_url'], headers={"Accept": "application/vnd.github+json", "X-GitHub-Api-Version": "2022-11-28", "Authorization": f"Bearer {token}"}).json()

        if artifacts['total_count'] == 0:
            break

        for artifact in artifacts['artifacts']:
            if artifact['name'] != 'ccdec-bin':
                continue

            r = requests.get(artifact['archive_download_url'], headers={"Accept": "application/vnd.github+json", "X-GitHub-Api-Version": "2022-11-28", "Authorization": f"Bearer {token}"}, stream=True)

            if not os.path.exists("/opt/cros-codecs"):
                os.mkdir("/opt/cros-codecs")

            with open("/opt/cros-codecs/ccdec.zip", 'wb') as fd:
                for chunk in r.iter_content(chunk_size=128):
                    fd.write(chunk)

            subprocess.run(['unzip', '/opt/cros-codecs/ccdec.zip', '-d', '/opt/cros-codecs/'])
            os.chmod("/opt/cros-codecs/ccdec", mode=(stat.S_IRWXU | stat.S_IRWXG | stat.S_IRWXO))
            os.environ['PATH'] = os.environ['PATH'] + ":/opt/cros-codecs"

            found = True

            break

        break

    return found

def retrieve_ccdec(run_id, repo, token):
    # Retrieve built sha and branch
    run = requests.get(URL_RUN.format(run_id=run_id, repo=repo), headers={"Accept": "application/vnd.github+json", "X-GitHub-Api-Version": "2022-11-28", "Authorization": f"Bearer {token}"}).json()
    sha = run['head_sha']
    branch = run['head_branch']

    # Retrieve the artifact
    for i in range(30):
        try:
            if retrieve_ccdec_github(sha, branch, repo, token):
                break
            time.sleep(10)
        except Exception as e:
            print(e)


argparser = argparse.ArgumentParser()
argparser.add_argument('--arch', choices=['amd', 'intel'], help='Architecture', required=True)
argparser.add_argument('--config-file', help='Configuration file', required=True)
argparser.add_argument('--ccdec-build-id', help='ccded binary build id', required=True)
argparser.add_argument('--token', help='Github read token', required=True)
argparser.add_argument('--repo', help='Github git repository', required=True)
argparser.add_argument('--single', help='Run in a single thread', action='store_true')
args = argparser.parse_args()

retrieve_ccdec(args.ccdec_build_id, args.repo, args.token)

with open(args.config_file, "r") as stream:
    try:
        config = yaml.safe_load(stream)
        for arch, arch_info in config.items():
            if arch != args.arch:
                continue
            device_type=arch_info['device_type']
            for c in arch_info['codecs']:
                for codec, test_suites in c.items():
                    for ts in test_suites['test-suites']:
                        for test_suite in ts:
                            skips=ts[test_suite]["skip-vectors"]
                            run_fluster(codec, test_suite, skips, args.single)
            break
    except yaml.YAMLError as exc:
        print(exc)
