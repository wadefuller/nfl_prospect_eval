import { useState, useEffect, useCallback } from "react";
import type { Meta, Prospect, YearData, SortField, SortDir, ProspectComp } from "./types";
import { Header } from "./components/Header";
import { ProspectTable } from "./components/ProspectTable";
import { ModelPage } from "./components/ModelPage";
import { InspectorPage } from "./components/InspectorPage";

function useHashRoute() {
  const [hash, setHash] = useState(() => window.location.hash || "#/");
  useEffect(() => {
    const onHash = () => setHash(window.location.hash || "#/");
    window.addEventListener("hashchange", onHash);
    return () => window.removeEventListener("hashchange", onHash);
  }, []);
  return hash;
}

export default function App() {
  const route = useHashRoute();
  const onModelPage = route === "#/model";
  const onInspectorPage = route === "#/inspector";
  const onProspectsPage = !onModelPage && !onInspectorPage;

  const [meta, setMeta] = useState<Meta | null>(null);

  // Single-year mode
  const [year, setYear] = useState<number>(2026);
  const [prospects, setProspects] = useState<Prospect[]>([]);
  const [loadedYear, setLoadedYear] = useState<number | null>(null);

  // All-classes mode
  const [allClasses, setAllClasses] = useState(false);
  const [allProspects, setAllProspects] = useState<Prospect[]>([]);
  const [allLoaded, setAllLoaded] = useState(false);

  // Shared UI state
  const [posFilter, setPosFilter] = useState<string>("ALL");
  const [sortField, setSortField] = useState<SortField>("pick");
  const [sortDir, setSortDir] = useState<SortDir>("asc");
  const [expandedId, setExpandedId] = useState<string | null>(null);
  const [comps, setComps] = useState<Record<string, ProspectComp[]>>({});

  // Load meta on mount
  useEffect(() => {
    fetch("/data/meta.json")
      .then((r) => r.json())
      .then((m: Meta) => {
        setMeta(m);
        setYear(m.availableYears[m.availableYears.length - 1]);
      });
  }, []);

  // Load single year data — tag each prospect with its draft_year
  useEffect(() => {
    if (!year || allClasses || !onProspectsPage) return;
    let cancelled = false;
    fetch(`/data/prospects/${year}.json`)
      .then((r) => r.json())
      .then((d: YearData) => {
        if (cancelled) return;
        setProspects(d.prospects.map((p) => ({
          ...p,
          draft_year: d.draftYear,
          ppg_if_hit: p.p_made_it > 0 ? p.exp_ppg / p.p_made_it : p.exp_ppg,
        })));
        setLoadedYear(d.draftYear);
        setExpandedId(null);
      });
    return () => { cancelled = true; };
  }, [year, allClasses, onProspectsPage]);

  // Load all years when entering all-classes mode
  useEffect(() => {
    if (!allClasses || !meta || !onProspectsPage) return;
    let cancelled = false;
    Promise.all(
      meta.availableYears.map((yr) =>
        fetch(`/data/prospects/${yr}.json`)
          .then((r) => r.json())
          .then((d: YearData) => d.prospects.map((p) => ({
            ...p,
            draft_year: d.draftYear,
            ppg_if_hit: p.p_made_it > 0 ? p.exp_ppg / p.p_made_it : p.exp_ppg,
          })))
      )
    ).then((results) => {
      if (cancelled) return;
      setAllProspects(results.flat());
      setAllLoaded(true);
    });
    return () => { cancelled = true; };
  }, [allClasses, meta, onProspectsPage]);

  const handleAllClassesToggle = useCallback(() => {
    setAllClasses((prev) => {
      if (!prev) {
        setSortField("prospect_score");
        setSortDir("desc");
        setExpandedId(null);
      } else {
        setSortField("pick");
        setSortDir("asc");
        setExpandedId(null);
      }
      return !prev;
    });
  }, []);

  // Load comps on demand
  const loadComps = useCallback(
    (id: string) => {
      if (comps[id]) return;
      fetch(`/data/comps/${id}.json`)
        .then((r) => r.json())
        .then((d: { prospectId: string; comps: ProspectComp[] }) => {
          setComps((prev) => ({ ...prev, [id]: d.comps }));
        });
    },
    [comps]
  );

  const handleExpand = useCallback(
    (id: string) => {
      if (expandedId === id) {
        setExpandedId(null);
      } else {
        setExpandedId(id);
        loadComps(id);
      }
    },
    [expandedId, loadComps]
  );

  const handleSort = useCallback(
    (field: SortField) => {
      if (sortField === field) {
        setSortDir((d) => (d === "asc" ? "desc" : "asc"));
      } else {
        setSortField(field);
        setSortDir(field === "name" ? "asc" : "desc");
      }
    },
    [sortField]
  );

  // Filter and sort from the appropriate source
  const source = allClasses ? allProspects : prospects;
  const loading = !allClasses && loadedYear !== year;
  const allLoading = allClasses && !allLoaded;
  const filtered = source
    .filter((p) => posFilter === "ALL" || p.position === posFilter)
    .sort((a, b) => {
      const dir = sortDir === "asc" ? 1 : -1;
      const av = (a[sortField as keyof Prospect] as number | string) ?? -999;
      const bv = (b[sortField as keyof Prospect] as number | string) ?? -999;
      if (typeof av === "string" && typeof bv === "string")
        return av.localeCompare(bv) * dir;
      return ((av as number) - (bv as number)) * dir;
    });

  if (!meta) {
    return (
      <div style={{ display: "flex", alignItems: "center", justifyContent: "center", minHeight: "100vh" }}>
        <div style={{ color: "#4A5578", fontFamily: "var(--font-mono)", fontSize: 14 }}>Loading…</div>
      </div>
    );
  }

  const isLoading = allClasses ? allLoading : loading;

  return (
    <div style={{ maxWidth: 1400, margin: "0 auto", paddingBottom: 48 }}>
      <Header
        years={meta.availableYears}
        year={year}
        onYearChange={(y) => {
          setYear(y);
          if (allClasses) {
            setAllClasses(false);
            setSortField("pick");
            setSortDir("asc");
          }
        }}
        posFilter={posFilter}
        onPosFilterChange={setPosFilter}
        allClasses={allClasses}
        onAllClassesToggle={handleAllClassesToggle}
        totalProspects={filtered.length}
        lastUpdated={meta.lastUpdated}
        route={route}
      />

      {onModelPage ? (
        <ModelPage />
      ) : onInspectorPage ? (
        <InspectorPage />
      ) : isLoading ? (
        <div style={{ display: "flex", alignItems: "center", justifyContent: "center", padding: "80px 0" }}>
          <div style={{ color: "#4A5578", fontFamily: "var(--font-mono)", fontSize: 13 }}>
            {allClasses ? "Loading all draft classes…" : "Loading prospects…"}
          </div>
        </div>
      ) : (
        <ProspectTable
          prospects={filtered}
          sortField={sortField}
          sortDir={sortDir}
          onSort={handleSort}
          expandedId={expandedId}
          onExpand={handleExpand}
          comps={comps}
          draftYear={year}
          allClasses={allClasses}
        />
      )}
    </div>
  );
}
