import click

@click.command()
@click.argument('input')
def main(input):
  raw = '\\'
  click.echo(input.replace(raw, '\\\\'))

if __name__ == '__main__':
  main()
