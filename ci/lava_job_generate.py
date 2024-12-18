#!/bin/env python3

import argparse
import jinja2
import os
import yaml

def get_device_type(arch, config_file):
    with open(config_file, "r") as stream:
        try:
            config = yaml.safe_load(stream)
            return config[arch]['device_type']
        except yaml.YAMLError as exc:
            print(exc)

def main():
    argparser = argparse.ArgumentParser()
    argparser.add_argument('--template', help='Input template file', required=True)
    argparser.add_argument('--test-branch', help='The branch being tested', default='main')
    argparser.add_argument('--test-repo', help='The repository being tested', required=True)
    argparser.add_argument('--arch', choices=['amd', 'intel'], help='Architecture', required=True)
    argparser.add_argument('--ccdec-build-id', help='ccdec build id', required=True)
    argparser.add_argument('--token', help='Github read token', required=True)
    argparser.add_argument('--repo', help='Github repository', required=True)
    argparser.add_argument('--config-file', help='Configuration file', required=True)
    args = argparser.parse_args()

    env = jinja2.Environment(loader=jinja2.FileSystemLoader(os.path.dirname(args.template)),
                             undefined=jinja2.StrictUndefined)

    template = env.get_template(os.path.basename(args.template))

    print(template.render(ccdec_build_id=args.ccdec_build_id, arch=args.arch, device_type=get_device_type(args.arch, args.config_file), test_branch=args.test_branch, repo_url=args.test_repo, token=args.token, repo=args.repo))


if __name__ == '__main__':
    main()

