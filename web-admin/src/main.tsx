import React from "react";
import ReactDOM from "react-dom/client";
import { BrowserRouter } from "react-router-dom";
import { ConfigProvider, App as AntdApp } from "antd";
import App from "./App";
import "antd/dist/reset.css";
import "./index.css";

ReactDOM.createRoot(document.getElementById("root")!).render(
  <React.StrictMode>
    <ConfigProvider>
      <AntdApp>
        <BrowserRouter basename="/admin/ui">
          <App />
        </BrowserRouter>
      </AntdApp>
    </ConfigProvider>
  </React.StrictMode>,
);
