/* =============================================================================
 * Custom D3.js spike map (must-have feature 7)
 * Each spike stands on a county; its height is the number of deaths there.
 * Inspired by the D3 Spike Map example (https://observablehq.com/@d3/spike-map/2),
 * written for this project and driven live by the dashboard filters:
 * the Shiny server sends county totals with the message "spikeData".
 * Map shapes: us-atlas states-10m.json (US Census Bureau cartographic boundaries).
 * ========================================================================== */
(function () {
  const W = 975, H = 610;
  const BLUE = "#2a78d6";
  const projection = d3.geoAlbersUsa().scale(1300).translate([W / 2, H / 2]);
  const path = d3.geoPath(projection);
  let gMap, gSpikes, gLegend, tip, ready = false, pending = null;
  let zt = d3.zoomIdentity;                     // current zoom transform

  const spike = (len, w = 7) => `M${-w / 2},0L0,${-len}L${w / 2},0`;

  function setup() {
    const root = d3.select("#spikemap");
    if (root.empty()) { setTimeout(setup, 200); return; }
    // SPIKE_BASE: folder prefix, set only by code/graphs/graph08_spikemap.R
    d3.json((window.SPIKE_BASE || "") + "states-10m.json").then(us => {
      const svg = root.append("svg")
        .attr("viewBox", [0, 0, W, H])
        .attr("role", "img")
        .attr("aria-label", "Spike map of road deaths by county")
        .style("width", "100%").style("height", "auto");
      gMap = svg.append("g");
      gMap.append("path")
        .datum(topojson.feature(us, us.objects.nation))
        .attr("fill", "#efeee9").attr("d", path);
      gMap.append("path")
        .datum(topojson.mesh(us, us.objects.states, (a, b) => a !== b))
        .attr("fill", "none").attr("stroke", "#ffffff")
        .attr("stroke-width", 1).attr("vector-effect", "non-scaling-stroke").attr("d", path);
      gSpikes = svg.append("g");
      gLegend = svg.append("g").attr("transform", `translate(${W - 90},${H - 30})`);
      svg.append("text").attr("x", W - 58).attr("y", H - 30 - 190).attr("fill", "#52514e")
        .attr("font-size", 12).attr("text-anchor", "middle").text("Deaths");
      // Navigation (Interaction lecture; Exercise 7 extension): zoom and pan with
      // d3.zoom. The map is scaled, but spikes are only moved, so their height
      // still matches the legend. Double-click resets the view.
      const zoom = d3.zoom().scaleExtent([1, 8])
        .translateExtent([[0, 0], [W, H]])
        .on("zoom", event => {
          zt = event.transform;
          gMap.attr("transform", zt);
          gSpikes.selectAll("path").attr("transform", d => `translate(${zt.apply([d.x, d.y])})`);
        });
      svg.call(zoom).on("dblclick.zoom", () => svg.transition().duration(500).call(zoom.transform, d3.zoomIdentity));
      svg.append("text").attr("x", 8).attr("y", H - 10).attr("fill", "#52514e").attr("font-size", 12)
        .text("Scroll to zoom, drag to pan, double-click to reset");
      tip = root.append("div").attr("class", "spike-tip").style("opacity", 0);
      ready = true;
      if (pending) draw(pending);
    });
  }

  function draw(msg) {
    if (!ready) { pending = msg; return; }
    const c = msg.rows;
    const n = c.deaths ? c.deaths.length : 0;
    const rows = [];
    for (let i = 0; i < n; i++) {
      const p = projection([c.lon[i], c.lat[i]]);
      if (!p) continue;                         // Puerto Rico is outside the projection
      rows.push({ fips: c.county_fips[i], name: c.county[i], deaths: c.deaths[i],
                  crashes: c.crashes[i], x: p[0], y: p[1] });
    }
    rows.sort((a, b) => a.y - b.y);             // draw back to front
    const max = d3.max(rows, d => d.deaths) || 1;
    const len = d3.scaleLinear([0, max], [0, 180]);

    gSpikes.selectAll("path")
      .data(rows, d => d.fips)
      .join("path")
      .attr("transform", d => `translate(${zt.apply([d.x, d.y])})`)
      .attr("fill", BLUE).attr("fill-opacity", 0.3)
      .attr("stroke", BLUE).attr("stroke-width", 0.7)
      .on("mouseenter", function (event, d) {
        d3.select(this).attr("fill-opacity", 0.85);
        tip.style("opacity", 1)
           .html(`<b>${d.name}</b><br>${d.deaths} death${d.deaths === 1 ? "" : "s"} in ${d.crashes} crash${d.crashes === 1 ? "" : "es"}`);
      })
      .on("mousemove", function (event) {
        const box = document.getElementById("spikemap").getBoundingClientRect();
        tip.style("left", (event.clientX - box.left + 12) + "px")
           .style("top", (event.clientY - box.top - 10) + "px");
      })
      .on("mouseleave", function () {
        d3.select(this).attr("fill-opacity", 0.3);
        tip.style("opacity", 0);
      })
      .transition().duration(500)
      .attr("d", d => spike(len(d.deaths)));

    // legend: three spikes with round values
    const ticks = len.ticks(4).filter(t => t > 0).slice(-3);
    const lg = gLegend.selectAll("g").data(ticks, d => d).join(enter => {
      const g = enter.append("g");
      g.append("path").attr("fill", BLUE).attr("fill-opacity", 0.3)
       .attr("stroke", BLUE).attr("stroke-width", 0.7);
      g.append("text").attr("dy", "1.3em").attr("text-anchor", "middle")
       .attr("fill", "#52514e").attr("font-size", 12);
      return g;
    });
    lg.attr("transform", (d, i) => `translate(${i * 32},0)`);
    lg.select("path").attr("d", d => spike(len(d)));
    lg.select("text").text(d => d3.format(",")(d));
  }

  function init() {
    setup();
    Shiny.addCustomMessageHandler("spikeData", draw);
  }
  if (window.Shiny && Shiny.addCustomMessageHandler) init();
  else document.addEventListener("DOMContentLoaded", init);
})();
