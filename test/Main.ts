import * as d3 from "d3-scale";

export function scaleLinear_<Range>(
  domain: [number, number],
  range: [Range, Range]
) {
  return (d: number) =>
    d3.scaleLinear<Range, number>().domain(domain).range(range)(d);
}
