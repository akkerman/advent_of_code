#!/bin/env python3
import requests
from dotenv import load_dotenv
import argparse
import os

load_dotenv('.cookie')

def getenv(var: str) -> str:
    """ Get environment variable, or rise ValueError if not set """
    value = os.getenv(var) # type: ignore
    if not value:
        raise ValueError(f'Missing required environment variable: {var}.')
    return value # type: ignore




def handle_input(year:int, day:int):
    cookie = getenv('cookie')
    response = requests.get(f"https://adventofcode.com/{year}/day/{day}/input", headers={"cookie": cookie})
    print(response.text, end='')

def handle_example(year:int, day:int):
    cookie = getenv('cookie')
    response = requests.get(f"https://adventofcode.com/{year}/day/{day}", headers={"cookie": cookie})
    result = response.text.split('<pre><code>')[1].split('</code></pre>')[0]
    print(result, end='')

def handle_title(year:int, day:int):
    cookie = getenv('cookie')
    response = requests.get(f"https://adventofcode.com/{year}/day/{day}", headers={"cookie": cookie})
    result = response.text.split('<h2>')[1].split('</h2>')[0]
    output_str = f'"""{result.strip("-").strip()}."""'
    print(output_str, end='')


def main():
    parser = argparse.ArgumentParser(
        description="Advent of Code helper script"
    )
    # Subcommands: 'input' en 'example'
    subparsers = parser.add_subparsers(
        title="commands", dest="command", required=True
    )

    # 'input' subcommand
    input_parser = subparsers.add_parser(
        "input", help="Process input data"
    )
    input_parser.add_argument(
        "year", type=int, help="Year of the puzzle"
    )
    input_parser.add_argument(
        "day", type=int, help="Day of the puzzle"
    )

    # 'example' subcommand
    example_parser = subparsers.add_parser(
        "example", help="Process example data"
    )
    example_parser.add_argument(
        "year", type=int, help="Year of the puzzle"
    )
    example_parser.add_argument(
        "day", type=int, help="Day of the puzzle"
    )

    # 'input' subcommand
    title_parser = subparsers.add_parser(
        "title", help="Process title data"
    )
    title_parser.add_argument(
        "year", type=int, help="Year of the puzzle"
    )
    title_parser.add_argument(
        "day", type=int, help="Day of the puzzle"
    )

    args = parser.parse_args()

    if args.command == "input":
        handle_input(args.year, args.day)
    elif args.command == "example":
        handle_example(args.year, args.day)
    elif args.command == "title":
        handle_title(args.year, args.day)

if __name__ == "__main__":
    main()
