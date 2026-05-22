import { useState } from "react";
import { Card, Form, Input, Button, Typography, App } from "antd";
import { useNavigate } from "react-router-dom";
import { setToken, clearToken } from "../api/client";
import axios from "axios";

export default function Login() {
  const nav = useNavigate();
  const { message } = App.useApp();
  const [loading, setLoading] = useState(false);

  const onFinish = async (vals: { token: string }) => {
    setLoading(true);
    try {
      setToken(vals.token.trim());
      await axios.get("/admin/healthz", {
        headers: { Authorization: `Bearer ${vals.token.trim()}` },
        timeout: 5_000,
      });
      message.success("Authenticated");
      nav("/users");
    } catch (e: any) {
      clearToken();
      const status = e?.response?.status;
      if (status === 401 || status === 403) {
        message.error("Token rejected — check value and try again");
      } else {
        message.error(`Login failed: ${e?.message ?? "unknown error"}`);
      }
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="flex items-center justify-center min-h-screen bg-gray-100">
      <Card style={{ width: 420 }}>
        <Typography.Title level={3} style={{ marginTop: 0 }}>
          mcpserver admin
        </Typography.Title>
        <Typography.Paragraph type="secondary">
          Paste your bootstrap admin token (from{" "}
          <code>MCPSERVER_ADMIN_TOKEN</code>) or an admin user's JWT.
        </Typography.Paragraph>
        <Form layout="vertical" onFinish={onFinish}>
          <Form.Item
            label="Token"
            name="token"
            rules={[{ required: true, message: "Token is required" }]}
          >
            <Input.Password
              placeholder="mcp_..."
              autoFocus
              data-testid="login-token"
            />
          </Form.Item>
          <Form.Item>
            <Button
              type="primary"
              htmlType="submit"
              loading={loading}
              block
              data-testid="login-submit"
            >
              Sign in
            </Button>
          </Form.Item>
        </Form>
      </Card>
    </div>
  );
}
