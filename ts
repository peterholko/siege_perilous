#!/bin/bash
cd sp_ts
npm run dev
cp dist/sp2.js ../priv/static
cp index.html ../priv
