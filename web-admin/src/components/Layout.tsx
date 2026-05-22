import { Layout as AntLayout, Menu, Button, Typography, App } from "antd";
import { Outlet, useNavigate, useLocation } from "react-router-dom";
// Per-icon ESM imports keep Vite's tree-shaker from pulling the whole
// @ant-design/icons index in.
import UserOutlined from "@ant-design/icons/es/icons/UserOutlined";
import LogoutOutlined from "@ant-design/icons/es/icons/LogoutOutlined";
import { clearToken } from "../api/client";

export default function Layout() {
  const nav = useNavigate();
  const loc = useLocation();
  const { message } = App.useApp();

  const selected = loc.pathname.startsWith("/users") ? ["users"] : [];

  const onLogout = () => {
    clearToken();
    message.success("Logged out");
    nav("/login");
  };

  return (
    <AntLayout className="min-h-screen">
      <AntLayout.Sider theme="light" width={220}>
        <div className="p-4 text-lg font-semibold">mcpserver admin</div>
        <Menu
          mode="inline"
          selectedKeys={selected}
          items={[
            {
              key: "users",
              icon: <UserOutlined />,
              label: "Users",
              onClick: () => nav("/users"),
            },
          ]}
        />
      </AntLayout.Sider>
      <AntLayout>
        <AntLayout.Header
          className="flex items-center justify-end"
          style={{ background: "#fff", paddingRight: 24 }}
        >
          <Button icon={<LogoutOutlined />} onClick={onLogout}>
            Log out
          </Button>
        </AntLayout.Header>
        <AntLayout.Content className="p-6">
          <Outlet />
        </AntLayout.Content>
        <AntLayout.Footer style={{ textAlign: "center" }}>
          <Typography.Text type="secondary">mcpserver-r</Typography.Text>
        </AntLayout.Footer>
      </AntLayout>
    </AntLayout>
  );
}
