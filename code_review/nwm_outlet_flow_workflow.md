# NWM outlet-flow workflow

This workflow treats each `comid` in [random_forest_data_new.csv](/Users/myer056/GitHub/rc_sfa-rc-3-wenas-meta/code_review/random_forest_data_new.csv) as a watershed outlet reach in the NWM routing network and extracts NWM retrospective `streamflow` from the hourly `CHRTOUT` files.

The script is [nwm_outlet_flow_workflow.R](/Users/myer056/GitHub/rc_sfa-rc-3-wenas-meta/code_review/nwm_outlet_flow_workflow.R). It currently targets the NOAA NWM retrospective v2.1 archive, which NOAA documents as covering February 1979 through December 2020 in hourly channel-routing output.

Run examples:

```bash
Rscript nwm_outlet_flow_workflow.R --start-year 2010 --end-year 2010
Rscript nwm_outlet_flow_workflow.R --start-year 2008 --end-year 2014 --output nwm_2008_2014.csv
Rscript nwm_outlet_flow_workflow.R --start-year 2010 --end-year 2010 --max-hours 24
```

Outputs:

- `nwm_outlet_flow_summary.csv`: one row per site and year with mean annual streamflow in `m3/s`
- `nwm_outlet_flow_summary_period.csv`: one row per site for the full requested period

Important caveats:

- The script computes means from hourly NWM `streamflow` values at each outlet COMID. It does not calculate basin-average runoff or other watershed-area fluxes.
- It skips missing or unreachable hourly files rather than stopping immediately.
- If a COMID is absent from the NWM routing network, that site will return `NA`.
- A full multi-year run will make many HTTP requests and can take a while.

Official references used for this workflow:

- NOAA NWM overview: https://water.noaa.gov/about/nwm
- NOAA NWM retrospective archive on AWS: https://registry.opendata.aws/nwm-archive/
- NOAA NODD NWM data documentation: https://github.com/NOAA-Big-Data-Program/nodd-data-docs/blob/main/nwm/README.md
